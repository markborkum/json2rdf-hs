{-# LANGUAGE DeriveDataTypeable #-}

import           Control.Monad (forever)
import qualified Data.Aeson as JS
import           Data.Aeson.Parser (value')
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as B8
import           Data.Canonical (canonicalize)
import           Data.Described (Described(describeWith))
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

data JSON2RDF = Transform { file_ :: FilePath }
              | Describe  { file_ :: FilePath , intersection_ :: Bool , pretty_ :: Bool }
                deriving (Eq, Show, Data, Typeable)

mode :: Mode (CmdArgs JSON2RDF)
mode =
  cmdArgsMode
    $ modes [ Transform { file_ = fileFlags def
                        } &= help "Transform JSON document(s) into RDF graph"
                          &= auto
            , Describe  { file_         = fileFlags def
                        , intersection_ = def &= name "I" &= help "Use \"intersection\" operator (instead of \"union\")"
                        , pretty_       = def &= name "P" &= help "Format output using pretty-printer"
                        } &= help "Print description of JSON document structure"
            ] &= program "json2rdf"
              &= summary "JSON2RDF v0.0.1, (C) Mark Borkum <m.i.borkum@soton.ac.uk>"
  where
    fileFlags x =
      x &= args &= typFile

main :: IO ()
main = do
  args <- cmdArgsRun mode
  
  text <- Data.Text.IO.readFile (file_ args)
  let expr = fmap canonicalize (Data.Attoparsec.Text.parseOnly expr' text)
  either (\l -> hPutStrLn stderr l >> exitFailure) (\r -> go1 args r >> exitSuccess) expr
  
    where
      
      go1 :: JSON2RDF -> Expression -> IO ()
      go1 (Transform _) =
        forever . go2
      go1 (Describe _ bool1 bool2) = do
        putStrLn_pp_DescriptorTree bool2 . opts_describeWith bool1
      
      go2 :: Expression -> IO ()
      go2 expr = do
        currentTime <- getCurrentTime
        currentLine <- B8.getLine
        let v = Data.Attoparsec.ByteString.parseOnly value' currentLine
        let ts = js2rdf (Just currentTime) expr (eitherToMaybe v)
        putStrLn_pp_RDFTriple ts
      
      eitherToMaybe :: Either a b -> Maybe b
      eitherToMaybe =
        either (const Nothing) Just
      
      opts_describeWith :: Bool -> Expression -> D.DescriptorTree T.Text ()
      opts_describeWith True =
        describeWith D.intersection maxBound
      opts_describeWith False =
        describeWith D.union minBound
      
      putStrLn_pp_DescriptorTree :: (Show k, Show v) => Bool -> D.DescriptorTree k v -> IO ()
      putStrLn_pp_DescriptorTree b =
        putStrLn . render . D.pp_DescriptorTree b
      
      putStrLn_pp_RDFTriple :: S.Set RDFTriple -> IO ()
      putStrLn_pp_RDFTriple =
        mapM_ (putStrLn . render . pp_RDFTriple) . S.toList
      
      pp_RDFTriple :: RDFTriple -> Doc
      pp_RDFTriple (s, p, o) =
        pPrint s <+> pPrint p <+> pPrint o <+> char '.'
