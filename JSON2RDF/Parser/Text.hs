module JSON2RDF.Parser.Text
( expr'
) where

import Control.Monad (liftM, liftM2)
import Control.Applicative ((*>))
import Data.Attoparsec.Combinator (choice, many', manyTill, option, sepBy, sepBy1)
import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.Monoid (mempty, mappend, mconcat)

import qualified Data.Aeson as JS
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T

import JSON2RDF.Types

expr' :: P.Parser Expression
expr' = do
  option mempty (P.string (T.pack "#!") *> manyTill P.anyChar (P.try P.endOfLine))
  ws_
  expr <- prelude_
  ws_
  exprs <- sepBy expr_ ws_
  ws_
  P.endOfInput
  return (expr `mappend` mconcat exprs)

---------------------------------------------------------------------------------

prelude_ :: P.Parser Expression
prelude_ = do
  expr <- option mempty baseDecl_
  ws_
  exprs <- sepBy prefixDecl_ ws_
  return (expr `mappend` mconcat exprs)

baseDecl_ :: P.Parser Expression
baseDecl_ = do
  caseInsensitiveKeyword "BASE"
  ws_
  createRDFLabel <- uriResource_
  return (SetNamespace Nothing createRDFLabel)

prefixDecl_ :: P.Parser Expression
prefixDecl_ = do
  caseInsensitiveKeyword "PREFIX"
  ws_
  key <- prefix_
  ws_
  P.char ':'
  ws_
  createRDFLabel <- uriResource_
  return (SetNamespace key createRDFLabel)

expr_ :: P.Parser Expression
expr_ =
  choice [block_, bindDecl_, conditional_, foreachExpr_, matchExpr_, triplesExpr_]

block_ :: P.Parser Expression
block_ = do
  P.char '{'
  ws_
  exprs <- sepBy expr_ ws_
  ws_
  P.char '}'
  return (ScopeExpression (mconcat exprs))

bindDecl_ :: P.Parser Expression
bindDecl_ = do
  caseInsensitiveKeyword "BIND"
  ws_
  key <- bareword_
  ws_
  P.char ':'
  ws_
  createRDFLabel <- choice [literal_, uriResource_]
  return (SetRDFLabel key createRDFLabel)

conditional_ :: P.Parser Expression
conditional_ = do
  (createBool, whenTrue) <- if_ "IF"
  ws_
  xs <- sepBy (if_ "ELSIF") ws_
  ws_
  x <- option mempty (else_ "ELSE")
  let whenFalse = foldl (flip (uncurry Conditional)) x xs
  return (Conditional createBool whenTrue whenFalse)
    where
      if_ cs = do
        caseInsensitiveKeyword cs
        ws_
        createBool <- conditionalExpr_
        ws_
        expr <- expr_
        return (createBool, expr)
      else_ cs = do
        caseInsensitiveKeyword cs
        ws_
        expr_
      conditionalExpr_ =
        conditionalOrExpr_
      conditionalOrExpr_ =
        liftM Disjunction (sepBy1 conditionalAndExpr_ (ws_ *> P.string (T.pack "||") *> ws_))
      conditionalAndExpr_ =
        liftM Conjunction (sepBy1 conditionalValueLogical_ (ws_ *> P.string (T.pack "&&") *> ws_))
      conditionalValueLogical_ =
        relationalExpr_
      relationalExpr_ = do
        p <- numericExpr_
        choice [equalsExpr_ p, notEqualsExpr_ p, return p]
          where
            equalsExpr_ p1 = do
              ws_
              P.string (T.pack "==")
              ws_
              p2 <- numericExpr_
              return (mkEqualsExpr_ p1 p2)
            notEqualsExpr_ p1 = do
              ws_
              P.string (T.pack "!=")
              ws_
              p2 <- numericExpr_
              return (mkNotEqualsExpr_ p1 p2)
            mkEqualsExpr_ =
              on Equals mkOperand_
            mkNotEqualsExpr_ =
              on NotEquals mkOperand_
            mkOperand_ (ConstBool b) =
              Left (ConstJSValue (Just (JS.Bool b)))
            mkOperand_ (IsJSValueDefined createJSValue) =
              Left createJSValue
            mkOperand_ (IsRDFLabelDefined createRDFLabel) =
              Right createRDFLabel
            mkOperand_ _ =
              Left (ConstJSValue Nothing)
      numericExpr_ =
        additiveExpr_
      additiveExpr_ =
        multiplicativeExpr_
      multiplicativeExpr_ =
        unaryExpr_
      unaryExpr_ =
        choice [liftM Negation (P.char '!' *> ws_ *> unaryExpr_), primaryExpr_]
      primaryExpr_ =
        choice [brackettedExpr_, false_, true_, liftM IsJSValueDefined value_, liftM IsRDFLabelDefined (choice [literal_, resource_])]
      brackettedExpr_ = do
        P.char '('
        ws_
        p <- conditionalExpr_
        ws_
        P.char ')'
        return p
      false_ = do
        P.string (T.pack "false")
        return (ConstBool False)
      true_ = do
        P.string (T.pack "true")
        return (ConstBool True)

foreachExpr_ :: P.Parser Expression
foreachExpr_ = do
  caseInsensitiveKeyword "FOREACH"
  ws_
  createJSValue <- value_
  ws_
  expr <- block_
  return (ForeachJSValue createJSValue expr)

matchExpr_ :: P.Parser Expression
matchExpr_ = do
  caseInsensitiveKeyword "MATCH"
  ws_
  createPattern <- value_
  ws_
  caseInsensitiveKeyword "IN"
  ws_
  createJSValue <- value_
  ws_
  expr <- block_
  return (ForeachJSMatchRegexPOSIX createPattern createJSValue expr)

triplesExpr_ :: P.Parser Expression
triplesExpr_ =
  triplesSameSubject_

triplesSameSubject_ :: P.Parser Expression
triplesSameSubject_ = do
  s <- subject_
  ws_
  pos <- liftM concat (sepBy1 predicateObjectList_ (ws_ *> P.char ';' *> ws_))
  ws_
  P.char '.'
  -- return (mconcat (fmap (uncurry (InsertRDFTriple s)) pos))
  return (InsertRDFTriples s pos)

predicateObjectList_ :: P.Parser [(RDFLabelGenerator, RDFLabelGenerator)]
predicateObjectList_ = do
  p <- predicate_
  ws_
  os <- sepBy1 object_ (ws_ *> P.char ',' *> ws_)
  return (fmap ((,) p) os)

subject_ :: P.Parser RDFLabelGenerator
subject_ =
  uriResource_

predicate_ :: P.Parser RDFLabelGenerator
predicate_ =
  choice [uriResource_, owlSameAs_, rdfType_]
    where
      owlSameAs_ = do
        P.char '='
        return (ConstRDFLabel (Just (Res (T.pack "http://www.w3.org/2002/07/owl#sameAs"))))
      rdfType_ = do
        P.char 'a'
        return (ConstRDFLabel (Just (Res (T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))))

object_ :: P.Parser RDFLabelGenerator
object_ =
  choice [literal_, uriResource_]

value_ :: P.Parser JSValueGenerator
value_ =
  choice [ref_, false_, true_, null_, number_, interpolatedString_]
    where
      false_ = do
        P.string (T.pack "false")
        return (ConstJSValue (Just (JS.Bool False)))
      true_ = do
        P.string (T.pack "true")
        return (ConstJSValue (Just (JS.Bool True)))
      null_ = do
        P.string (T.pack "null")
        return (ConstJSValue Nothing)
      number_ = do
        n <- P.number
        return (ConstJSValue (Just (JS.Number n)))

ref_ :: P.Parser JSValueGenerator
ref_ = do
  P.char '$'
  P.char '{'
  ws_
  createJSValue <- choice [varRef_, valueRef_]
  ws_
  P.char '}'
  return createJSValue
    where
      varRef_ =
        liftM (\(GetRDFLabel key) -> LookupRDFLabel key) var_
      valueRef_ =
        liftM (LookupJSValue . (:) JSId) (sepBy1 (choice [this_, urlDecode_, urlEncode_, decode_, encode_, reverse_, length_, match_, strip_, downcase_, upcase_, strftime_, quoted_, unquoted_]) (ws_ *> P.char '/' *> ws_))
      this_ = do
        P.char '.'
        return JSId
      urlDecode_ = do
        P.string (T.pack "unescape")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSURLDecode
      urlEncode_ = do
        P.string (T.pack "escape")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSURLEncode
      decode_ = do
        P.string (T.pack "decode")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSDecode
      encode_ = do
        P.string (T.pack "encode")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSEncode
      reverse_ = do
        P.string (T.pack "reverse")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSReverseArray
      length_ = do
        P.string (T.pack "length")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSLength
      match_ = do
        P.string (T.pack "match")
        ws_
        P.char '('
        ws_
        pattern <- string_
        ws_
        P.char ')'
        return (JSMatchRegexPOSIX (T.unpack pattern))
      strip_ = do
        P.string (T.pack "strip")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSStrip
      downcase_ = do
        P.string (T.pack "downcase")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSToLower
      upcase_ = do
        P.string (T.pack "upcase")
        ws_
        P.char '('
        ws_
        P.char ')'
        return JSToUpper
      strftime_ = do
        P.string (T.pack "strftime")
        ws_
        P.char '('
        ws_
        parseFormat <- string_
        ws_
        P.char ','
        ws_
        formatFormat <- string_
        ws_
        P.char ')'
        return (JSTime (T.unpack parseFormat) (T.unpack formatFormat))
      quoted_ = do
        text <- string_
        return (JSLookupObject text)
      unquoted_ = do
        text <- bareword_
        let ints = reads (T.unpack text) :: [(Int, String)]
        return (if length ints == 0 then JSLookupObject text else JSLookupArray (fst (head ints)))

uriResource_ :: P.Parser RDFLabelGenerator
uriResource_ =
  choice [resource_, anon_, collection_]
    where
      anon_ = do
        P.char '['
        ws_
        pos <- liftM concat (sepBy predicateObjectList_ (ws_ *> P.char ';' *> ws_))
        ws_
        P.char ']'
        return (AsBlank pos)
      collection_ = do
        P.char '('
        ws_
        os <- sepBy object_ ws_
        ws_
        P.char ')'
        return (foldr (\o acc -> AsBlank [(rdfFirst_, o), (rdfRest_, acc)]) rdfNil_ os)
          where
            rdfFirst_ =
              ConstRDFLabel (Just (Res (T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")))
            rdfRest_ =
              ConstRDFLabel (Just (Res (T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")))
            rdfNil_ =
              ConstRDFLabel (Just (Res (T.pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")))

resource_ :: P.Parser RDFLabelGenerator
resource_ =
  choice [var_, qname_, interpolatedUriref_]
    where
      qname_ = do
        key <- prefix_
        ws_
        P.char ':'
        ws_
        choice [qnameBareword_ key, qnameValue_ key]
      qnameBareword_ key1 = do
        key2 <- bareword_
        return (AsQName key1 (ConstJSValue (Just (JS.String key2))))
      qnameValue_ key = do
        createJSValue <- interpolatedString_
        return (AsQName key createJSValue)

var_ :: P.Parser RDFLabelGenerator
var_ = do
  P.char '?'
  ws_
  key <- bareword_
  return (GetRDFLabel key)

literal_ :: P.Parser RDFLabelGenerator
literal_ =
  choice [now_, go]
    where
      now_ = do
        P.string (T.pack "now")
        return AsNow
      go = do
        v <- value_
        ws_
        choice [langLiteral_ v, typedLiteral_ v, otherwise_ v]
      langTag_ = do
        text <- P.takeWhile1 (P.inClass "a-zA-Z")
        texts <- many' (liftM2 (T.append . T.singleton) (P.char '-') (P.takeWhile1 (P.inClass "a-zA-Z0-9")))
        return (mconcat (text:texts))
      langLiteral_ v = do
        P.char '@'
        ws_
        langTag <- choice [literal_, liftM (AsLit . ConstJSValue . Just . JS.String) langTag_]
        return (AsLangLit (AsLit v) langTag)
      typedLiteral_ v = do
        P.string (T.pack "^^")
        ws_
        createRDFLabel <- resource_
        return (AsTypedLit (AsLit v) createRDFLabel)
      otherwise_ v =
        return (AsLit v)

interpolatedString_ :: P.Parser JSValueGenerator
interpolatedString_ = do
  P.char '"'
  createJSValueList <- many' (choice [ref_, f (P.takeWhile1 (P.notInClass ['\\', '"', '$'])), f qpair_])
  P.char '"'
  return (ConcatJSValue createJSValueList)
    where
      f =
        liftM (ConstJSValue . Just . JS.String)
      qpair_ = do
        char1 <- P.char '\\'
        char2 <- P.anyChar
        return (T.pack [char1, char2])

interpolatedUriref_ :: P.Parser RDFLabelGenerator
interpolatedUriref_ = do
  P.char '<'
  createJSValueList <- many' (choice [ref_, f (P.takeWhile1 (P.notInClass ['\\', '>', '$'])), f qpair_])
  P.char '>'
  return (AsRes (ConcatJSValue createJSValueList))
    where
      f =
        liftM (ConstJSValue . Just . JS.String)
      qpair_ = do
        char1 <- P.char '\\'
        char2 <- P.anyChar
        return (T.pack [char1, char2])

string_ :: P.Parser T.Text
string_ = do
  P.char '"'
  texts <- many' (choice [P.takeWhile1 (P.notInClass ['\\', '"']), qpair_])
  P.char '"'
  return (mconcat texts)
    where
      qpair_ = do
        char1 <- P.char '\\'
        char2 <- P.anyChar
        return (T.pack [char1, char2])

bareword_ :: P.Parser T.Text
bareword_ =
  liftM2 T.append (P.takeWhile1 (P.inClass "a-zA-Z0-9")) (P.takeWhile (P.inClass "-_a-zA-Z0-9"))

prefix_ :: P.Parser (Maybe T.Text)
prefix_ =
  liftM go pn_prefix_
    where
      pn_prefix_ =
        choice [liftM T.singleton (P.char '_'), bareword_]
      go text
        | T.null text || (T.singleton '_' == text) = Nothing
        | otherwise                                = Just text

ws_ :: P.Parser ()
ws_ = do
  P.skipSpace
  sepBy (choice [commentBlock_, commentLine_]) P.skipSpace
  P.skipSpace
  return ()
    where
      commentBlock_ =
        P.string (T.pack "/*") *> manyTill P.anyChar (P.try (P.string (T.pack "*/")))
      commentLine_ =
        P.string (T.pack "//") *> manyTill P.anyChar (P.try P.endOfLine)

caseInsensitiveKeyword :: String -> P.Parser ()
caseInsensitiveKeyword =
  mapM_ (\c -> choice [P.char (toLower c), P.char (toUpper c)])
