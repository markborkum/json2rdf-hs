module JSON2RDF.RDF.Graph where

import Data.Attoparsec.Number (Number(..))
import Control.Monad (MonadPlus(mzero))
import Text.PrettyPrint.HughesPJ ((<>), char, int, text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

import qualified Data.Aeson as JS
import qualified Data.Set as S
import qualified Data.Text as T

data RDFLabel = Res T.Text
              | PlainLit T.Text (Maybe T.Text)
              | TypedLit T.Text T.Text
              | Blank !Int
                deriving (Eq, Ord, Read, Show)

type RDFTriple = (RDFLabel, RDFLabel, RDFLabel)

type RDFGraph = S.Set RDFTriple

getResourceURI :: (MonadPlus m) => RDFLabel -> m T.Text
getResourceURI (Res uri) =
  return uri
getResourceURI _ =
  mzero

getLiteralText :: (MonadPlus m) => RDFLabel -> m T.Text
getLiteralText (PlainLit val _) =
  return val
getLiteralText (TypedLit val _) =
  return val
getLiteralText _ =
  mzero

getLiteralLanguageTag :: (MonadPlus m) => RDFLabel -> m (Maybe T.Text)
getLiteralLanguageTag (PlainLit _ langTag) =
  return langTag
getLiteralLanguageTag _ =
  mzero

getLiteralDatatype :: (MonadPlus m) => RDFLabel -> m T.Text
getLiteralDatatype (TypedLit _ dType) =
  return dType
getLiteralDatatype _ =
  mzero

setResourceURI :: (MonadPlus m) => T.Text -> RDFLabel -> m RDFLabel
setResourceURI uri (Res _) =
  return (Res uri)
setResourceURI _ _ =
  mzero

setLiteralText :: (MonadPlus m) => T.Text -> RDFLabel -> m RDFLabel
setLiteralText val (PlainLit _ langTag) =
  return (PlainLit val langTag)
setLiteralText val (TypedLit _ dType) =
  return (TypedLit val dType)
setLiteralText _ _ =
  mzero

setLiteralLanguageTag :: (MonadPlus m) => Maybe T.Text -> RDFLabel -> m RDFLabel
setLiteralLanguageTag langTag (PlainLit val _) =
  return (PlainLit val langTag)
setLiteralLanguageTag langTag (TypedLit val _) =
  return (PlainLit val langTag)
setLiteralLanguageTag _ _ =
  mzero

setLiteralDatatype :: (MonadPlus m) => T.Text -> RDFLabel -> m RDFLabel
setLiteralDatatype dType (PlainLit val _) =
  return (TypedLit val dType)
setLiteralDatatype dType (TypedLit val _) =
  return (TypedLit val dType)
setLiteralDatatype _ _ =
  mzero

class ToResource a where
  toResource :: (MonadPlus m) => a -> m RDFLabel

instance ToResource JS.Value where
  toResource (JS.String text) =
    return (Res text)
  toResource _ =
    mzero

class ToLiteral a where
  toLiteral :: (MonadPlus m) => a -> m RDFLabel

instance ToLiteral JS.Value where
  toLiteral (JS.Bool bool) =
    return (TypedLit (T.toLower (T.pack (show bool))) (T.pack "http://www.w3.org/2001/XMLSchema#boolean"))
  toLiteral (JS.Number (D double)) =
    return (TypedLit (T.pack (show double)) (T.pack "http://www.w3.org/2001/XMLSchema#double"))
  toLiteral (JS.Number (I int)) =
    return (TypedLit (T.pack (show int)) (T.pack "http://www.w3.org/2001/XMLSchema#integer"))
  toLiteral (JS.String text) =
    return (PlainLit text Nothing)
  toLiteral _ =
    mzero

instance Pretty RDFLabel where
  pPrint (Res uri) =
    char '<' <> text (T.unpack uri) <> char '>'
  pPrint (PlainLit val langTag) =
    let doc1 = text (show (T.unpack val))
        go string = if any (== '"') string then show string else string
    in  maybe doc1 ((doc1 <> char '@') <>) . fmap (text . go . T.unpack) $ langTag
  pPrint (TypedLit val dType) =
    text (show (T.unpack val))
      <> text "^^"
      <> pPrint (Res dType)
  pPrint (Blank n) =
    text "_:b" <> int n
