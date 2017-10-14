{-# LANGUAGE RecordWildCards #-}

module Tika where

import           Cli
import           Data.List         (isPrefixOf)
import           GHC.IO.Exception
import           Prelude.Unicode
import           System.Environment (lookupEnv)
import           System.Process    (readProcessWithExitCode)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core (getText, hread, runLA, (//>), (>>>))


tikaClass ∷ String
tikaClass = "org.apache.tika.cli.TikaCLI"


runTika ∷ Options → IO (ExitCode, String, String)
runTika Options{..} = do
  classpath <- lookupEnv "CLASSPATH"
  case (classpath, tikaJarPath) of
    (_, Just path) -> runWith ["-jar", path]
    (Just _,    _) -> runWith [tikaClass]
    _              ->
      return ( ExitFailure 1
             , ""
             , "Either CLASSPATH or '--tika-jar' should be defined!" )
  where
    runWith params =
      readProcessWithExitCode
        javaExecutable
        (params ++ ["--html", inputFilePath])
        ""


paragraphs ∷ String → [String]
paragraphs html =
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter (not ∘ isPdfMetadata) allParagraphs


isPdfMetadata ∷ String → Bool
isPdfMetadata text =
  "<<\n" `isPrefixOf` text
