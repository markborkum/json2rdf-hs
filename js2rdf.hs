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

data JS2RDF = Transform { file_ :: FilePath }
            | Show      { file_ :: FilePath }
            | Canon     { file_ :: FilePath }
            | Describe  { file_ :: FilePath , intersection_ :: Bool , pretty_ :: Bool }
              deriving (Eq, Show, Data, Typeable)

mode :: Mode (CmdArgs JS2RDF)
mode =
	cmdArgsMode
		$ modes [ Transform { file_ = fileFlags def
												} &= help "Transform JSON document(s) into RDF graph"
													&= auto
						, Show      { file_ = fileFlags def
												} &= help "Print s-expression version of named input FILE"
						, Canon     { file_ = fileFlags def
												} &= help "Print canonical version of named input FILE"
						, Describe  { file_         = fileFlags def
												, intersection_ = def &= name "I" &= help "Intersection (instead of union)"
												, pretty_       = def &= name "P" &= help "Format output using pretty-printer"
												} &= help "Print description of JSON document structure"
						] &= program "js2rdf"
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
			
			go1 :: JS2RDF -> Expression -> IO ()
			go1 (Transform _) =
				forever . go2
			go1 (Show _) =
				putStrLn . show
			go1 (Canon _) =
				putStrLn . render . pPrint
			go1 (Describe _ bool1 bool2) =
				putStrLn . render . D.pp_DescriptorTree bool2 . (if bool1
					then describeWith D.intersection (maxBound :: D.DescriptorTree T.Text ())
					else describeWith D.union (minBound :: D.DescriptorTree T.Text ()))
			
			go2 :: Expression -> IO ()
			go2 expr = do
				bs <- B8.getLine
				let v = either (const Nothing) Just (Data.Attoparsec.ByteString.parseOnly value' bs)
				time <- getCurrentTime
				let ts = js2rdf (Just time) expr v
				mapM_ (putStrLn . render . pp_RDFTriple) (S.toList ts)
			
			pp_RDFTriple :: RDFTriple -> Doc
			pp_RDFTriple (s, p, o) =
				pPrint s <+> pPrint p <+> pPrint o <+> char '.'
