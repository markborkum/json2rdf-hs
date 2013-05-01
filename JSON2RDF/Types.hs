{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module JSON2RDF.Types
( js2rdf
, JSKey(..) , FromJSKey(..)
, JSValueGenerator(..) , RDFLabelGenerator(..) , Predicate(..) , Expression(..)
, module JSON2RDF.RDF.Graph
) where

import Prelude hiding (foldr)

import Control.Arrow (first)
import Control.Monad ((>=>), join, liftM, liftM2, liftM3, MonadPlus(mzero))
import Control.Monad.RWS.Lazy.Evaluable (EvaluableT(..))
import Data.Aeson.Parser (value')
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.Number (Number(..))
import Data.Canonical (Canonical(canonicalize))
import Data.Described (Described(describeWith))
import Data.Foldable (Foldable(foldMap, foldr))
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..), All(..), Any(..))
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import Network.HTTP.Base (urlEncode, urlDecode)
import System.Locale (defaultTimeLocale)
import Text.PrettyPrint.HughesPJ ((<>), (<+>), Doc, empty, braces, brackets, char, comma, colon, doubleQuotes, hcat, hsep, int, parens, space, text, zeroWidthText)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))
import Text.Regex.Posix ((=~))

import qualified Data.Aeson as JS
import qualified Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.DescriptorTree as D
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import JSON2RDF.RDF.Graph

js2rdf :: Maybe UTCTime -> Expression -> Maybe JS.Value -> RDFGraph
js2rdf time f v =
  (\((), _, ts) -> ts) . evaluate f . setJSValue v . setCurrentTime time $ emptyContext

-------------------------------------------------------------------------------

data Context = Context { getJSValue             :: Maybe JS.Value
                       , getNamespaces          :: M.Map (Maybe T.Text) T.Text
                       , getRDFLabels           :: M.Map T.Text (Maybe RDFLabel)
                       , getBlankRDFLabelsCount :: !Int
                       , getCurrentTime         :: Maybe UTCTime
                       } deriving (Eq, Show)

emptyContext :: Context
emptyContext =
  Context { getJSValue             = Nothing
          , getNamespaces          = M.empty
          , getRDFLabels           = M.empty
          , getBlankRDFLabelsCount = 0
          , getCurrentTime         = Nothing
          }

setJSValue :: Maybe JS.Value -> Context -> Context
setJSValue x ctx =
  ctx { getJSValue = x }

setNamespaces :: M.Map (Maybe T.Text) T.Text -> Context -> Context
setNamespaces x ctx =
  ctx { getNamespaces = x }

setRDFLabels :: M.Map T.Text (Maybe RDFLabel) -> Context -> Context
setRDFLabels x ctx =
  ctx { getRDFLabels = x }

setBlankRDFLabelsCount :: Int -> Context -> Context
setBlankRDFLabelsCount x ctx =
  ctx { getBlankRDFLabelsCount = x }

setCurrentTime :: Maybe UTCTime -> Context -> Context
setCurrentTime x ctx =
  ctx { getCurrentTime = x }

withJSValue :: (Maybe JS.Value -> Maybe JS.Value) -> Context -> Context
withJSValue f =
  f . getJSValue >>= setJSValue

withNamespaces :: (M.Map (Maybe T.Text) T.Text -> M.Map (Maybe T.Text) T.Text) -> Context -> Context
withNamespaces f =
  f . getNamespaces >>= setNamespaces

withRDFLabels :: (M.Map T.Text (Maybe RDFLabel) -> M.Map T.Text (Maybe RDFLabel)) -> Context -> Context
withRDFLabels f =
  f . getRDFLabels >>= setRDFLabels

withBlankRDFLabelsCount :: (Int -> Int) -> Context -> Context
withBlankRDFLabelsCount f =
  f . getBlankRDFLabelsCount >>= setBlankRDFLabelsCount

withCurrentTime :: (Maybe UTCTime -> Maybe UTCTime) -> Context -> Context
withCurrentTime f =
  f . getCurrentTime >>= setCurrentTime

insertOrDeleteNamespace :: Maybe T.Text -> Maybe T.Text -> Context -> Context
insertOrDeleteNamespace key =
  withNamespaces . liftM2 maybe M.delete M.insert key

insertOrDeleteRDFLabel :: T.Text -> Maybe (Maybe RDFLabel) -> Context -> Context
insertOrDeleteRDFLabel key =
  withRDFLabels . liftM2 maybe M.delete M.insert key

newBlankRDFLabel :: Context -> (Maybe RDFLabel, Context)
newBlankRDFLabel =
  liftM2 (,) (Just . Blank . getBlankRDFLabelsCount) (withBlankRDFLabelsCount succ)

lookupRDFLabel :: T.Text -> Context -> (Maybe RDFLabel, Context)
lookupRDFLabel key =
  join (maybe ((\(lb, ctx) -> (lb, insertOrDeleteRDFLabel key (Just lb) ctx)) . newBlankRDFLabel) (,) . M.lookup key . getRDFLabels)

nowRDFLabel :: Context -> Maybe RDFLabel
nowRDFLabel =
  maybe Nothing (Just . go) . getCurrentTime
    where
      go time =
        TypedLit (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%T%z" time)) (T.pack "http://www.w3.org/2001/XMLSchema#dateTime")

-------------------------------------------------------------------------------

data JSKey = JSId
           | JSLength
           | JSLookupArray !Int
           | JSLookupObject T.Text
           | JSMatchRegexPOSIX String
           | JSStrip
           | JSToLower
           | JSToUpper
           | JSTime String String
           | JSDecode
           | JSEncode
           | JSURLDecode
           | JSURLEncode
             deriving (Eq, Ord, Read, Show)

isIdempotent :: JSKey -> Bool
isIdempotent JSId =
  True
isIdempotent JSStrip =
  True
isIdempotent JSToLower =
  True
isIdempotent JSToUpper =
  True
isIdempotent _ =
  False

class FromJSKey a where
  fromJSKey :: (MonadPlus m) => a -> JS.Value -> m JS.Value

instance FromJSKey () where
  fromJSKey () =
    const mzero

instance (Foldable t, FromJSKey a) => FromJSKey (t a) where
  fromJSKey =
    foldr (\a acc v -> fromJSKey a v >>= acc) return

instance FromJSKey JSKey where
  fromJSKey JSId v =
    return v
  fromJSKey JSLength (JS.String text) =
    return . JS.Number . I . toInteger . T.length $ text
  fromJSKey JSLength (JS.Array vector) =
    return . JS.Number . I . toInteger . V.length $ vector
  fromJSKey JSLength (JS.Object hashMap) =
    return . JS.Number . I . toInteger . HM.size $ hashMap
  fromJSKey JSLength _ =
    return . JS.Number . I $ 1
  fromJSKey (JSLookupArray idx) (JS.Array vector)
    | (idx >= 0) && (idx < V.length vector) =
      V.indexM vector idx
    | (idx < 0) && (idx >= (negate . V.length) vector) =
      V.indexM vector (idx + V.length vector)
    | otherwise =
      mzero
  fromJSKey (JSLookupArray _) _ =
    mzero
  fromJSKey (JSLookupObject key) (JS.Object hashMap) =
    maybe mzero return . HM.lookup key $ hashMap
  fromJSKey (JSLookupObject _) _ =
    mzero
  fromJSKey (JSMatchRegexPOSIX pattern) (JS.String text) =
    go (T.unpack text =~ pattern)
      where
        go [] =
          mzero
        go xs =
          let f = fmap (JS.Array . V.fromList) . fmap
          in  return (f (f (JS.String . T.pack)) xs)
  fromJSKey (JSMatchRegexPOSIX _) _ =
    mzero
  fromJSKey JSStrip (JS.String text) =
    return . JS.String . T.strip $ text
  fromJSKey JSStrip _ =
    mzero
  fromJSKey JSToLower (JS.String text) =
    return . JS.String . T.toLower $ text
  fromJSKey JSToLower _ =
    mzero
  fromJSKey JSToUpper (JS.String text) =
    return . JS.String . T.toUpper $ text
  fromJSKey JSToUpper _ =
    mzero
  fromJSKey (JSTime parseFormat formatFormat) (JS.String text) =
    let locale = defaultTimeLocale
        time = (parseTime locale parseFormat (T.unpack text) :: Maybe UTCTime)
    in  maybe mzero (return . JS.String . T.pack . formatTime locale formatFormat) time
  fromJSKey (JSTime _ _) _ =
    mzero
  fromJSKey JSDecode (JS.String text) =
    either (const mzero) return . parseOnly value' . encodeUtf8 $ text
  fromJSKey JSDecode _ =
    mzero
  fromJSKey JSEncode v =
    return . JS.String . T.pack . B8.unpack . JS.encode $ v
  fromJSKey JSURLDecode (JS.String text) =
    return . JS.String . T.pack . urlDecode . T.unpack $ text
  fromJSKey JSURLDecode _ =
    mzero
  fromJSKey JSURLEncode (JS.String text) =
    return . JS.String . T.pack . urlEncode . T.unpack $ text
  fromJSKey JSURLEncode _ =
    mzero

-------------------------------------------------------------------------------

data JSValueGenerator = ConstJSValue (Maybe JS.Value)
                      | ConcatJSValue [JSValueGenerator]
                      | LookupJSValue [JSKey]
                      | LookupRDFLabel T.Text
                        deriving (Eq, Show)

data RDFLabelGenerator = ConstRDFLabel (Maybe RDFLabel)
                       | GetRDFLabel T.Text
                       | AsRes JSValueGenerator
                       | AsLit JSValueGenerator
                       | AsLangLit RDFLabelGenerator RDFLabelGenerator
                       | AsTypedLit RDFLabelGenerator RDFLabelGenerator
                       | AsQName (Maybe T.Text) JSValueGenerator
                       | AsBlank [(RDFLabelGenerator, RDFLabelGenerator)]
                       | AsNow
                         deriving (Eq, Show)

data Predicate = ConstBool Bool
               | IsJSValueDefined JSValueGenerator
               | IsRDFLabelDefined RDFLabelGenerator
               | Equals (Either JSValueGenerator RDFLabelGenerator) (Either JSValueGenerator RDFLabelGenerator)
               | NotEquals (Either JSValueGenerator RDFLabelGenerator) (Either JSValueGenerator RDFLabelGenerator)
               | Negation Predicate
               | Disjunction [Predicate]
               | Conjunction [Predicate]
                 deriving (Eq, Show)

data Expression = InsertRDFTriple RDFLabelGenerator RDFLabelGenerator RDFLabelGenerator
                | InsertRDFTriples RDFLabelGenerator [(RDFLabelGenerator, RDFLabelGenerator)]
                | Conditional Predicate Expression Expression
                | ForeachJSValue JSValueGenerator Expression
                | ForeachJSMatchRegexPOSIX JSValueGenerator JSValueGenerator Expression
                | ScopeExpression Expression
                | SequenceExpression [Expression]
                | SetJSValue JSValueGenerator
                | SetNamespace (Maybe T.Text) RDFLabelGenerator
                | SetRDFLabel T.Text RDFLabelGenerator
                  deriving (Eq, Show)

instance Monoid Expression where
  mempty =
    SequenceExpression []
  -- (SequenceExpression xs) `mappend` (SequenceExpression ys) =
  --  SequenceExpression (xs ++ ys)
  x `mappend` (SequenceExpression ys) =
    SequenceExpression (x : ys)
  -- (SequenceExpression xs) `mappend` y =
  --  SequenceExpression (xs ++ [y])
  x `mappend` y =
    SequenceExpression [x, y]
  mconcat =
    SequenceExpression

-------------------------------------------------------------------------------

instance EvaluableT JSValueGenerator RDFGraph Context (Maybe JS.Value) where
  evaluateT (ConstJSValue v) =
    evaluateT (const v :: Context -> Maybe JS.Value)
  evaluateT (ConcatJSValue createJSValueList) =
    liftM (\(text, ctx, ts) -> (liftM JS.String text, ctx, ts)) . evaluateTFoldr (\(v :: Maybe JS.Value) -> liftM2 T.append (v >>= (toLiteral >=> getLiteralText))) (return T.empty) createJSValueList
  evaluateT (LookupJSValue key) =
    evaluateT ((getJSValue >=> fromJSKey key) >>= (,))
  evaluateT (LookupRDFLabel key) =
    evaluateT (first go . lookupRDFLabel key)
      where
        go (Just (Res uri)) =
          Just (JS.String uri)
        go (Just (PlainLit text _)) =
          Just (JS.String text)
        go (Just (TypedLit text _)) =
          Just (JS.String text)
        go _ =
          Nothing

instance EvaluableT RDFLabelGenerator RDFGraph Context (Maybe RDFLabel) where
  evaluateT (ConstRDFLabel lb) =
    evaluateT (const lb :: Context -> Maybe RDFLabel)
  evaluateT (GetRDFLabel key) =
    evaluateT (lookupRDFLabel key)
  evaluateT (AsRes createJSValue) =
    liftM (\(v :: Maybe JS.Value, ctx, ts) -> (v >>= toResource, ctx, ts)) . evaluateT createJSValue
  evaluateT (AsLit createJSValue) =
    liftM (\(v :: Maybe JS.Value, ctx, ts) -> (v >>= toLiteral, ctx, ts)) . evaluateT createJSValue
  evaluateT (AsLangLit createRecipient createDonor) =
    \ctx1 -> do
      (lb1, ctx2, ts1) <- evaluateT createRecipient ctx1
      (lb2, ctx3, ts2) <- evaluateT createDonor ctx2
      let lb3 = maybe lb1 ((>>=) lb1 . setLiteralLanguageTag . Just) (lb2 >>= getLiteralText)
      return (lb3, ctx3, ts1 `mappend` ts2)
  evaluateT (AsTypedLit createRecipient createDonor) =
    \ctx1 -> do
      (lb1, ctx2, ts1) <- evaluateT createRecipient ctx1
      (lb2, ctx3, ts2) <- evaluateT createDonor ctx2
      let lb3 = maybe lb1 ((>>=) lb1 . setLiteralDatatype) (lb2 >>= getResourceURI)
      return (lb3, ctx3, ts1 `mappend` ts2)
  evaluateT (AsQName key createJSValue) =
    join (maybe whenNothing whenJust . M.lookup key . getNamespaces)
      where
        whenNothing =
          evaluateT (const Nothing :: Context -> Maybe RDFLabel)
        whenJust uri =
          liftM (\(lb, ctx, ts) -> (lb >>= getLiteralText >>= Just . Res . T.append uri, ctx, ts)) . evaluateT (AsLit createJSValue)
  evaluateT (AsBlank createPredicateObjectList) =
    \ctx1 -> do
      (lb, ctx2, ts1) <- evaluateT newBlankRDFLabel ctx1
      ((), ctx3, ts2) <- evaluateT (InsertRDFTriples (ConstRDFLabel lb) createPredicateObjectList) ctx2
      return (lb, ctx3, ts1 `mappend` ts2)
  evaluateT AsNow =
    evaluateT nowRDFLabel

instance EvaluableT Predicate RDFGraph Context Bool where
  evaluateT (ConstBool bool) =
    evaluateT (const bool :: Context -> Bool)
  evaluateT (IsJSValueDefined createJSValue) =
    liftM (\(v :: Maybe JS.Value, ctx, ts) -> (isJSValueDefined v, ctx, ts)) . evaluateT createJSValue
      where
        isJSValueDefined =
          liftM2 (&&) isJust (/= Just JS.Null)
  evaluateT (IsRDFLabelDefined createRDFLabel) =
    liftM (\(lb :: Maybe RDFLabel, ctx, ts) -> (isRDFLabelDefined lb, ctx, ts)) . evaluateT createRDFLabel
      where
        isRDFLabelDefined =
          isJust
  evaluateT (Equals (Left createJSValue1) (Left createJSValue2)) =
    \ctx1 -> do
      (v1 :: Maybe JS.Value, ctx2, ts1) <- evaluateT createJSValue1 ctx1
      (v2 :: Maybe JS.Value, ctx3, ts2) <- evaluateT createJSValue2 ctx2
      return (v1 == v2, ctx3, ts1 `mappend` ts2)
  evaluateT (Equals (Right createRDFLabel1) (Right createRDFLabel2)) =
    \ctx1 -> do
      (lb1 :: Maybe RDFLabel, ctx2, ts1) <- evaluateT createRDFLabel1 ctx1
      (lb2 :: Maybe RDFLabel, ctx3, ts2) <- evaluateT createRDFLabel2 ctx2
      return (lb1 == lb2, ctx3, ts1 `mappend` ts2)
  evaluateT (Equals (Left createJSValue) (Right createRDFLabel)) =
    \ctx1 -> do
      (v :: Maybe JS.Value, ctx2, ts1) <- evaluateT createJSValue ctx1
      (lb :: Maybe RDFLabel, ctx3, ts2) <- evaluateT createRDFLabel ctx2
      return (go v lb, ctx3, ts1 `mappend` ts2)
    where
      go v lb@(Just (Res _)) =
        (v >>= toResource) == lb
      go v lb =
        (v >>= toLiteral) == lb
  evaluateT (Equals l@(Right _) r@(Left _)) =
    evaluateT (Equals r l)
  evaluateT (NotEquals (Left createJSValue1) (Left createJSValue2)) =
    \ctx1 -> do
      (v1 :: Maybe JS.Value, ctx2, ts1) <- evaluateT createJSValue1 ctx1
      (v2 :: Maybe JS.Value, ctx3, ts2) <- evaluateT createJSValue2 ctx2
      return (v1 /= v2, ctx3, ts1 `mappend` ts2)
  evaluateT (NotEquals (Right createRDFLabel1) (Right createRDFLabel2)) =
    \ctx1 -> do
      (lb1 :: Maybe RDFLabel, ctx2, ts1) <- evaluateT createRDFLabel1 ctx1
      (lb2 :: Maybe RDFLabel, ctx3, ts2) <- evaluateT createRDFLabel2 ctx2
      return (lb1 /= lb2, ctx3, ts1 `mappend` ts2)
  evaluateT (NotEquals (Left createJSValue) (Right createRDFLabel)) =
    \ctx1 -> do
      (v :: Maybe JS.Value, ctx2, ts1) <- evaluateT createJSValue ctx1
      (lb :: Maybe RDFLabel, ctx3, ts2) <- evaluateT createRDFLabel ctx2
      return (go v lb, ctx3, ts1 `mappend` ts2)
    where
      go v lb@(Just (Res _)) =
        (v >>= toResource) /= lb
      go v lb =
        (v >>= toLiteral) /= lb
  evaluateT (NotEquals l@(Right _) r@(Left _)) =
    evaluateT (NotEquals r l)
  evaluateT (Negation createBool) =
    liftM (\(bool, ctx, ts) -> (not bool, ctx, ts)) . evaluateT createBool
  evaluateT (Disjunction createBoolList) =
    liftM (\(m, ctx, ts) -> (getAny m, ctx, ts)) . evaluateTFoldMap Any createBoolList
  evaluateT (Conjunction createBoolList) =
    liftM (\(m, ctx, ts) -> (getAll m, ctx, ts)) . evaluateTFoldMap All createBoolList

instance EvaluableT Expression RDFGraph Context () where
  evaluateT (InsertRDFTriple createSubject createPredicate createObject) =
    \ctx1 -> do
      (s :: Maybe RDFLabel, ctx2, ts1) <- evaluateT createSubject ctx1
      (p :: Maybe RDFLabel, ctx3, ts2) <- evaluateT createPredicate ctx2
      (o :: Maybe RDFLabel, ctx4, ts3) <- evaluateT createObject ctx3
      let ts4 = maybe S.empty S.singleton $ liftM3 (,,) s p o
      return ((), ctx4, ts1 `mappend` ts2 `mappend` ts3 `mappend` ts4)
  evaluateT (InsertRDFTriples createSubject createPredicateObjectList) =
    \ctx1 -> do
      (s :: Maybe RDFLabel, ctx2, ts1) <- evaluateT createSubject ctx1
      ((), ctx3, ts2) <- evaluateT (foldMap (uncurry $ InsertRDFTriple (ConstRDFLabel s)) createPredicateObjectList) ctx2
      return ((), ctx3, ts1 `mappend` ts2)
  evaluateT (Conditional createBool whenTrue whenFalse) =
    \ctx1 -> do
      (bool, ctx2, ts1) <- evaluateT createBool ctx1
      ((), ctx3, ts2) <- evaluateT (if bool then whenTrue else whenFalse) ctx2
      return ((), ctx3, ts1 `mappend` ts2)
  evaluateT (ForeachJSValue createJSValue expr) =
    \ctx1 -> do
      (v, ctx2, ts1) <- evaluateT createJSValue ctx1
      ((), ctx3, ts2) <- evaluateT (mkOuterExpression v) ctx2
      return ((), ctx3, ts1 `mappend` ts2)
    where
      mkOuterExpression =
        SequenceExpression . fmap (ScopeExpression . mkInnerExpression) . returnJS
      mkInnerExpression =
        SequenceExpression . flip (:) [expr] . SetJSValue . ConstJSValue
      returnJS (Just (JS.Array vector)) =
        V.toList . V.map Just $ vector
      returnJS _ =
        []
  evaluateT (ForeachJSMatchRegexPOSIX createPattern createJSValue expr) =
    \ctx1 -> do
      (v :: Maybe JS.Value, ctx2, ts1) <- evaluateT createPattern ctx1
      ((), ctx3, ts2) <- evaluateT (mkExpression (v >>= (toLiteral >=> getLiteralText))) ctx2
      return ((), ctx3, ts1 `mappend` ts2)
    where
      mkExpression (Just text) =
        ScopeExpression (SequenceExpression [SetJSValue createJSValue, ForeachJSValue (LookupJSValue [JSMatchRegexPOSIX (T.unpack text)]) expr])
      mkExpression _ =
        SequenceExpression []
  evaluateT (ScopeExpression expr) =
    \ctx1 ->
      liftM (\((), ctx2, ts) -> ((), setBlankRDFLabelsCount (getBlankRDFLabelsCount ctx2) ctx1, ts)) . evaluateT expr $ ctx1
  evaluateT (SequenceExpression exprs) =
    evaluateTFold exprs
  evaluateT (SetJSValue createJSValue) =
    liftM (\(v, ctx, ts) -> ((), setJSValue v ctx, ts)) . evaluateT createJSValue
  evaluateT (SetNamespace key createRDFLabel) =
    liftM (\(lb, ctx, ts) -> ((), insertOrDeleteNamespace key (lb >>= getResourceURI) ctx, ts)) . evaluateT createRDFLabel
  evaluateT (SetRDFLabel key createRDFLabel) =
    liftM (\(lb, ctx, ts) -> ((), insertOrDeleteRDFLabel key (Just lb) ctx, ts)) . evaluateT createRDFLabel

-------------------------------------------------------------------------------

instance Canonical JSValueGenerator where
  canonicalize (ConcatJSValue createJSValueList) =
    go3 . foldr go2 [] . foldr go1 [] . fmap canonicalize $ createJSValueList
      where
        go1 (ConcatJSValue xs) acc =
          xs ++ acc
        go1 x acc =
          x : acc
        go2 (ConstJSValue (Just (JS.String text1))) (ConstJSValue (Just (JS.String text2)) : acc) =
          ConstJSValue (Just (JS.String (text1 `mappend` text2))) : acc
        go2 x acc =
          x : acc
        go3 [t@(ConstJSValue (Just (JS.String _)))] =
          t
        go3 xs
          | null xs                            = ConstJSValue (Just (JS.String mempty))
          | any (== (ConstJSValue Nothing)) xs = ConstJSValue Nothing
          | otherwise                          = ConcatJSValue xs
  canonicalize (LookupJSValue key) =
    LookupJSValue (foldr go [] key)
      where
        go JSId acc =
          acc
        go x acc@(y:_)
          | isIdempotent y && x == y = acc
          | otherwise                = x : acc
        go x acc =
          x : acc
  canonicalize x =
    x

instance Canonical RDFLabelGenerator where
  canonicalize (AsRes createJSValue) =
    AsRes (canonicalize createJSValue)
  canonicalize (AsLit createJSValue) =
    AsLit (canonicalize createJSValue)
  canonicalize (AsLangLit createRecipient createDonor) =
    AsLangLit (canonicalize createRecipient) (canonicalize createDonor)
  canonicalize (AsTypedLit createRecipient createDonor) =
    AsTypedLit (canonicalize createRecipient) (canonicalize createDonor)
  canonicalize (AsQName key createJSValue) =
    AsQName key (canonicalize createJSValue)
  canonicalize (AsBlank createPredicateObjectList) =
    AsBlank (fmap (uncurry (on (,) canonicalize)) createPredicateObjectList)
  canonicalize x =
    x

instance Canonical Predicate where
  canonicalize (IsJSValueDefined createJSValue) =
    IsJSValueDefined (canonicalize createJSValue)
  canonicalize (IsRDFLabelDefined createRDFLabel) =
    IsRDFLabelDefined (canonicalize createRDFLabel)
  canonicalize (Equals l r) =
    on Equals go l r
      where
        go (Left createJSValue) =
          Left (canonicalize createJSValue)
        go (Right createRDFLabel) =
          Right (canonicalize createRDFLabel)
  canonicalize (NotEquals l r) =
    on NotEquals go l r
      where
        go (Left createJSValue) =
          Left (canonicalize createJSValue)
        go (Right createRDFLabel) =
          Right (canonicalize createRDFLabel)
  canonicalize (Negation (ConstBool False)) =
    ConstBool True
  canonicalize (Negation (ConstBool True)) =
    ConstBool False
  canonicalize (Negation (Negation createBool)) =
    canonicalize createBool
  canonicalize (Negation createBool) =
    Negation (canonicalize createBool)
  canonicalize (Disjunction createBoolList) =
    go2 . foldr go1 [] . fmap canonicalize $ createBoolList
      where
        go1 (Disjunction xs) =
          (xs ++)
        go1 x =
          (x :)
        go2 xs
          | null xs                      = ConstBool False
          | length xs == 1               = head xs
          | any (== (ConstBool True)) xs = ConstBool True
          | otherwise                    = Disjunction xs
  canonicalize (Conjunction createBoolList) =
    go2 . foldr go1 [] . fmap canonicalize $ createBoolList
      where
        go1 (Conjunction xs) =
          (xs ++)
        go1 x =
          (x :)
        go2 xs
          | null xs                       = ConstBool True
          | length xs == 1                = head xs
          | any (== (ConstBool False)) xs = ConstBool False
          | otherwise                     = Conjunction xs
  canonicalize x =
    x

instance Canonical Expression where
  canonicalize (InsertRDFTriple createSubject createPredicate createObject) =
    InsertRDFTriple (canonicalize createSubject) (canonicalize createPredicate) (canonicalize createObject)
  canonicalize (InsertRDFTriples createSubject createPredicateObjectList) =
    InsertRDFTriples (canonicalize createSubject) (fmap (uncurry (on (,) canonicalize)) createPredicateObjectList)
  canonicalize (Conditional createBool whenTrue whenFalse) =
    Conditional (canonicalize createBool) (canonicalize whenTrue) (canonicalize whenFalse)
  canonicalize (ForeachJSValue createJSValue expr) =
    ForeachJSValue (canonicalize createJSValue) (canonicalize expr)
  canonicalize (ForeachJSMatchRegexPOSIX createPattern createJSValue expr) =
    ForeachJSMatchRegexPOSIX (canonicalize createPattern) (canonicalize createJSValue) (canonicalize expr)
  canonicalize (ScopeExpression expr) =
    ScopeExpression (canonicalize expr)
  canonicalize (SequenceExpression exprs) =
    go2 . foldr go1 [] . fmap canonicalize $ exprs
      where
        go1 (ScopeExpression (SequenceExpression [])) =
          id
        go1 (SequenceExpression []) =
          id
        go1 (SequenceExpression [x]) =
          (x :)
        go1 (SequenceExpression xs) =
          (xs ++)
        go1 x =
          (x :)
        go2 xs
          | length xs == 1 = head xs
          | otherwise      = SequenceExpression xs
  canonicalize (SetJSValue createJSValue) =
    SetJSValue (canonicalize createJSValue)
  canonicalize (SetNamespace key createRDFLabel) =
    SetNamespace key (canonicalize createRDFLabel)
  canonicalize (SetRDFLabel key createRDFLabel) =
    SetRDFLabel key (canonicalize createRDFLabel)

-------------------------------------------------------------------------------

instance (Ord v) => Described (D.DescriptorTree T.Text v) JSValueGenerator where
  describeWith f z (ConcatJSValue createJSValueList) =
    describeWith f z createJSValueList
  describeWith _ z (LookupJSValue key) =
    foldr go z key
      where
        go (JSLookupArray idx) =
          D.HetListNode . return . (,) idx
        go (JSLookupObject key) =
          D.AscListNode . return . (,) key
        go _ =
          id
  describeWith _ z _ =
    z

instance (Ord v) => Described (D.DescriptorTree T.Text v) RDFLabelGenerator where
  describeWith f z (AsRes createJSValue) =
    describeWith f z createJSValue
  describeWith f z (AsLit createJSValue) =
    describeWith f z createJSValue
  describeWith f z (AsLangLit createRecipient createDonor) =
    describeWith f z createRecipient `f` describeWith f z createDonor
  describeWith f z (AsTypedLit createRecipient createDonor) =
    describeWith f z createRecipient `f` describeWith f z createDonor
  describeWith f z (AsQName _ createJSValue) =
    describeWith f z createJSValue
  describeWith f z (AsBlank createPredicateObjectList) =
    describeWith f z createPredicateObjectList
  describeWith _ z _ =
    z

instance (Ord v) => Described (D.DescriptorTree T.Text v) Predicate where
  describeWith f z (IsJSValueDefined createJSValue) =
    describeWith f z createJSValue
  describeWith f z (IsRDFLabelDefined createRDFLabel) =
    describeWith f z createRDFLabel
  describeWith f z (Equals l r) =
    describeWith f z l `f` describeWith f z r
  describeWith f z (NotEquals l r) =
    describeWith f z l `f` describeWith f z r
  describeWith f z (Negation createBool) =
    describeWith f z createBool
  describeWith f z (Disjunction createBoolList) =
    describeWith f z createBoolList
  describeWith f z (Conjunction createBoolList) =
    describeWith f z createBoolList
  describeWith _ z _ =
    z

instance (Ord v) => Described (D.DescriptorTree T.Text v) Expression where
  describeWith f z (InsertRDFTriple createSubject createPredicate createObject) =
    describeWith f z createSubject `f` describeWith f z createPredicate `f` describeWith f z createObject
  describeWith f z (InsertRDFTriples createSubject createPredicateObjectList) =
    describeWith f z createSubject `f` describeWith f z createPredicateObjectList
  describeWith f z (Conditional createBool whenTrue whenFalse) =
    describeWith f z createBool `f` describeWith f z whenTrue `f` describeWith f z whenFalse
  describeWith f z (ForeachJSValue createJSValue@(LookupJSValue _) expr) =
    describeWith f (D.HomListNode (describeWith f z expr)) createJSValue
  describeWith f z (ForeachJSValue createJSValue expr) =
    describeWith f z createJSValue `f` describeWith f z expr
  describeWith f z (ForeachJSMatchRegexPOSIX _ createJSValue _) =
    describeWith f z createJSValue
  describeWith f z (ScopeExpression expr) =
    describeWith f z expr
  describeWith f z (SequenceExpression exprs) =
    describeWith f z exprs
  describeWith f z (SetJSValue createJSValue) =
    describeWith f z createJSValue
  describeWith f z (SetNamespace _ createRDFLabel) =
    describeWith f z createRDFLabel
  describeWith f z (SetRDFLabel _ createRDFLabel) =
    describeWith f z createRDFLabel

---------------------------------------------------------------------------------

instance Pretty JSValueGenerator where
  pPrint (ConstJSValue (Just v)) =
    text . B8.unpack . JS.encode $ v
  pPrint (ConstJSValue Nothing) =
    pPrint (ConstJSValue (Just JS.Null))
  pPrint (ConcatJSValue createJSValueList) =
    doubleQuotes (hcat (fmap go createJSValueList))
      where
        go (ConstJSValue (Just (JS.String text'))) =
          text (T.unpack text')
        go (ConcatJSValue createJSValueList') =
          hcat (fmap go createJSValueList')
        go x =
          pPrint x
  pPrint (LookupJSValue key) =
    let key' = fmap go key
    in  char '$' <> braces (if null key' then char '.' else hcat (L.intersperse (char '/') key'))
      where
        go JSId =
          char '.'
        go JSLength =
          text "length" <> parens empty
        go (JSLookupArray idx) =
          int idx
        go (JSLookupObject key) =
          let string = T.unpack key in (if any ((==) '"') string then text (show string) else text string)
        go (JSMatchRegexPOSIX pattern) =
          text "match" <> parens (text (show pattern))
        go JSStrip =
          text "strip" <> parens empty
        go JSToLower =
          text "downcase" <> parens empty
        go JSToUpper =
          text "upcase" <> parens empty
        go (JSTime parseFormat formatFormat) =
          text "strftime" <> parens (text (show parseFormat) <> comma <+> text (show formatFormat))
        go JSDecode =
          text "decode" <> parens empty
        go JSEncode =
          text "encode" <> parens empty
        go JSURLDecode =
          text "urlDecode" <> parens empty
        go JSURLEncode =
          text "urlEncode" <> parens empty
  pPrint (LookupRDFLabel key) =
    pPrint (GetRDFLabel key)

instance Pretty RDFLabelGenerator where
  pPrint (ConstRDFLabel (Just lb@(Res uri)))
    | uri == T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" = char 'a'
    | uri == T.pack "http://www.w3.org/2002/07/owl#sameAs" = char '='
    | otherwise = pPrint lb
  pPrint (ConstRDFLabel (Just lb)) =
    pPrint lb
  pPrint (ConstRDFLabel Nothing) =
    pPrint (AsBlank [])
  pPrint (GetRDFLabel key) =
    char '?' <>
      text (T.unpack key)
  pPrint (AsRes (ConstJSValue (Just (JS.String text')))) =
    char '<' <> text (T.unpack text') <> char '>'
  pPrint (AsRes (ConcatJSValue createJSValueList)) =
    char '<' <> hcat (fmap go createJSValueList) <> char '>'
      where
        go (ConstJSValue (Just (JS.String text'))) =
          text (T.unpack text')
        go (ConcatJSValue createJSValueList') =
          hcat (fmap go createJSValueList')
        go x =
          pPrint x
  pPrint (AsRes createJSValue) =
    char '<' <> pPrint createJSValue <> char '>'
  pPrint (AsLit createJSValue) =
    pPrint createJSValue
  pPrint (AsLangLit createRecipient createDonor) =
    pPrint createRecipient <> char '@' <> pPrint createDonor
  pPrint (AsTypedLit createRecipient createDonor) =
    pPrint createRecipient <> text "^^" <> pPrint createDonor
  pPrint (AsQName (Just key) (ConstJSValue (Just (JS.String text')))) =
    text (T.unpack key) <> char ':' <> (let s = T.unpack text' in if any (== '"') s then text (show s) else text s)
  pPrint (AsQName (Just key) createJSValue) =
    text (T.unpack key) <> char ':' <> pPrint createJSValue
  pPrint (AsQName Nothing (ConstJSValue (Just (JS.String text')))) =
    text "_:" <> (let s = T.unpack text' in if any (== '"') s then text (show s) else text s)
  pPrint (AsQName Nothing createJSValue) =
    text "_:" <> pPrint createJSValue
  pPrint (AsBlank []) =
    brackets empty
  pPrint (AsBlank createPredicateObjectList) =
    brackets (space <> hsep (L.intersperse (text ";") (fmap (uncurry (on (<+>) pPrint)) createPredicateObjectList)) <> space)
  pPrint AsNow =
    text "now"

instance Pretty Predicate where
  pPrint (ConstBool False) =
    text "false"
  pPrint (ConstBool True) =
    text "true"
  pPrint (IsJSValueDefined createJSValue) =
    pPrint createJSValue
  pPrint (IsRDFLabelDefined createRDFLabel) =
    pPrint createRDFLabel
  pPrint (Equals l r) =
    either pPrint pPrint l <+> text "==" <+> either pPrint pPrint r
  pPrint (NotEquals l r) =
    either pPrint pPrint l <+> text "!=" <+> either pPrint pPrint r
  pPrint (Negation createBool) =
    char '!' <> pPrint createBool
  pPrint (Disjunction createBoolList) =
    parens (hsep (L.intersperse (text "||") (fmap pPrint createBoolList)))
  pPrint (Conjunction createBoolList) =
    parens (hsep (L.intersperse (text "&&") (fmap pPrint createBoolList)))

instance Pretty Expression where
  pPrint =
    pp 0
      where
        pp _ (InsertRDFTriple createSubject createPredicate createObject) =
          pPrint createSubject <+> pPrint createPredicate <+> pPrint createObject <+> char '.'
        pp _ (InsertRDFTriples _ []) =
          empty
        pp n (InsertRDFTriples createSubject createPredicateObjectList) =
          pPrint createSubject <> (hcat (L.intersperse (space <> char ';') (fmap go createPredicateObjectList))) <+> char '.'
            where
              go (createPredicate, createObject) =
                newline <> indent (succ n) <> pPrint createPredicate <+> pPrint createObject
        pp n (Conditional createBool whenTrue whenFalse) =
          text "IF"
            <+> pPrint createBool
            <+> whenTrueDoc
            <> whenTrueSpace
            <> whenFalseDoc
              where
                whenTrueDoc =
                  case whenTrue of
                    t@(ScopeExpression _) ->
                      pp n t
                    t ->
                      newline <> indent (succ n) <> pp n t <> newline <> indent n
                whenTrueSpace =
                  case whenTrue of
                    t@(ScopeExpression _) ->
                      space
                    _ ->
                      empty
                whenFalseDoc =
                  case whenFalse of
                    ScopeExpression (SequenceExpression []) ->
                      empty
                    SequenceExpression [] ->
                      empty
                    t@(ScopeExpression _) ->
                      text "ELSE" 
                        <+> pp n t
                    t ->
                      text "ELSE"
                        <+> newline <> indent (succ n) <> pp n t
        pp n (ForeachJSValue createJSValue expr) =
          text "FOREACH"
            <+> pPrint createJSValue
            <+> exprDoc
          where
            exprDoc =
              case expr of
                t@(ScopeExpression _) ->
                  pp n t
                t ->
                  newline <> indent (succ n) <> pp n t
        pp n (ForeachJSMatchRegexPOSIX createPattern createJSValue expr) =
          text "MATCH"
            <+> pPrint createPattern
            <+> text "IN"
            <+> pPrint createJSValue
            <+> exprDoc
          where
            exprDoc =
              case expr of
                t@(ScopeExpression _) ->
                  pp n t
                t ->
                  newline <> indent (succ n) <> pp n t
        pp n (ScopeExpression expr) =
          braces . wrapline n $ indent (succ n) <> pp (succ n) expr
        pp n (SequenceExpression exprs) =
          hcat . L.intersperse (newline <> indent n) . fmap (pp n) $ exprs
        pp _ (SetJSValue createJSValue) =
          text "CONTEXT" <> colon
            <+> pPrint createJSValue
        pp _ (SetNamespace (Just key) createRDFLabel) =
          text "PREFIX"
            <+> text (T.unpack key) <> colon
            <+> pPrint createRDFLabel
        pp _ (SetNamespace Nothing createRDFLabel) =
          text "PREFIX"
            <+> char '_' <> colon
            <+> pPrint createRDFLabel
        pp _ (SetRDFLabel key createRDFLabel) =
          text "BIND"
            <+> text (T.unpack key) <> colon
            <+> pPrint createRDFLabel
        indent :: Int -> Doc
        indent =
          hcat . flip replicate (space <> space)
        newline :: Doc
        newline =
          zeroWidthText "\n"
        wrapline :: Int -> Doc -> Doc
        wrapline n doc =
          newline <> doc <> newline <> indent n
