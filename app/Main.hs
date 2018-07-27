{-# LANGUAGE DeriveDataTypeable #-}

module Main
  ( main
  ) where

import           Control.Monad (forever)
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
              | Describe  { file_ :: FilePath , indent_ :: Bool , intersection_ :: Bool }
                deriving (Eq, Show, Data, Typeable)

mode :: Mode (CmdArgs JSON2RDF)
mode =
  cmdArgsMode
    $ modes [ Transform { file_         = fileFlags def
                        , slurp_        = def &= name "s" &= help "Read entire input stream (exactly once)"
                        } &= help "Transform JSON document(s) into RDF graph"
                          &= auto
            , Describe  { file_         = fileFlags def
                        , indent_       = def &= name "i" &= help "\"Pretty print\" output with proper indentation"
                        , intersection_ = def &= name "I" &= help "Use \"intersection\" operator (instead of \"union\")"
                        } &= help "Print description of JSON document structure"
            ] &= program "json2rdf"
              &= summary "JSON2RDF v0.0.2, (C) Mark Borkum <mark.borkum@pnnl.gov>"
  where
    fileFlags x =
      x &= args &= typFile

main :: IO ()
main = do
  args_ <- cmdArgsRun mode

  msgOrExpr <- fmap (fmap canonicalize . Data.Attoparsec.Text.parseOnly expr') (Data.Text.IO.readFile (file_ args_))
  either (\msg -> hPutStrLn stderr msg >> exitFailure) (\expr -> rpc args_ expr >> exitSuccess) msgOrExpr

    where

      rpc :: JSON2RDF -> Expression -> IO ()
      rpc (Transform _ True) =
        transform B8.getContents
      rpc (Transform _ False) =
        forever . transform B8.getLine
      rpc (Describe _ indent True) =
        fmap (pp_DescriptorTree indent) (describeWith D.intersection maxBound)
      rpc (Describe _ indent False) =
        fmap (pp_DescriptorTree indent) (describeWith D.union minBound)

      transform :: IO B8.ByteString -> Expression -> IO ()
      transform mv expr = do
        v <- mv >>= either (\msg -> hPutStrLn stderr msg >> return Nothing) (return . Just) . Data.Attoparsec.ByteString.parseOnly value'
        currentTime <- getCurrentTime
        let ts = js2rdf expr (Just currentTime) v
        mapM_ (putStrLn . render . pp_RDFTriple) (S.toList ts)

      pp_RDFTriple :: RDFTriple -> Doc
      pp_RDFTriple (s, p, o) =
        pPrint s <+> pPrint p <+> pPrint o <+> char '.'

      pp_DescriptorTree :: Bool -> D.DescriptorTree T.Text () -> IO ()
      pp_DescriptorTree indent =
        putStrLn . render . D.pp_DescriptorTree indent
