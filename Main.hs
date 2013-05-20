{-# LANGUAGE DeriveDataTypeable #-}

import           Control.Monad (forever)
import qualified Data.Aeson as JS
import           Data.Aeson.Parser (value')
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as B8
import           Data.Canonical (canonicalize)
import           Data.Described (describeWith)
import qualified Data.DescriptorTree as D
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO
import           Data.Time.Clock (getCurrentTime)
import           System.Console.CmdArgs
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hPutStrLn, stderr)
import           Text.PrettyPrint.HughesPJClass (pPrint, render, Doc, (<+>), char)

import JSON2RDF

data JSON2RDF = Transform { file_ :: FilePath , slurp_ :: Bool }
              | Describe  { file_ :: FilePath , intersection_ :: Bool , indent_ :: Bool }
                deriving (Eq, Show, Data, Typeable)

mode :: Mode (CmdArgs JSON2RDF)
mode =
  cmdArgsMode
    $ modes [ Transform { file_         = fileFlags def
                        , slurp_        = def &= name "s" &= help "Read entire input stream (exactly once)"
                        } &= help "Transform JSON document(s) into RDF graph"
                          &= auto
            , Describe  { file_         = fileFlags def
                        , intersection_ = def &= name "I" &= help "Use \"intersection\" operator (instead of \"union\")"
                        , indent_       = def &= name "i" &= help "\"Pretty print\" output with proper indentation"
                        } &= help "Print description of JSON document structure"
            ] &= program "json2rdf"
              &= summary "JSON2RDF v0.0.1, (C) Mark Borkum <m.i.borkum@soton.ac.uk>"
  where
    fileFlags x =
      x &= args &= typFile

main :: IO ()
main = do
  args <- cmdArgsRun mode
  
  msgOrExpr <- parseExpression (Data.Text.IO.readFile (file_ args))
  either (\msg -> hPutStrLn stderr msg >> exitFailure) (\expr -> rpc args expr >> exitSuccess) msgOrExpr
  
    where
      
      parseExpression :: IO T.Text -> IO (Either String Expression)
      parseExpression =
        (>>= return . fmap canonicalize . Data.Attoparsec.Text.parseOnly expr')
      
      parseValue :: IO B8.ByteString -> IO (Maybe JS.Value)
      parseValue =
        let logger = either (\msg -> hPutStrLn stderr msg >> return Nothing) (return . Just)
        in  (>>= logger . Data.Attoparsec.ByteString.parseOnly value')
      
      rpc :: JSON2RDF -> Expression -> IO ()
      rpc (Transform _ True) =
        transform B8.getContents
      rpc (Transform _ False) =
        forever . transform B8.getLine
      rpc (Describe _ bool1 bool2) =
        putStrLn . render . D.pp_DescriptorTree bool2 . describeWith' bool1
          where
            describeWith' :: Bool -> Expression -> D.DescriptorTree T.Text ()
            describeWith' True =
              describeWith D.intersection maxBound
            describeWith' False =
              describeWith D.union minBound
      
      transform :: IO B8.ByteString -> Expression -> IO ()
      transform mv expr = do
        v <- parseValue mv
        currentTime <- getCurrentTime
        let ts = js2rdf expr (Just currentTime) v
        mapM_ (putStrLn . render . pp_RDFTriple) (S.toList ts)
          where
            pp_RDFTriple :: RDFTriple -> Doc
            pp_RDFTriple (s, p, o) =
              pPrint s <+> pPrint p <+> pPrint o <+> char '.'
