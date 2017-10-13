{-# LANGUAGE UnicodeSyntax #-}

module Tika where

import           Data.List         (isPrefixOf)
import           GHC.IO.Exception
import           Prelude.Unicode
import           System.Environment (lookupEnv)
import           System.Process    (readProcessWithExitCode)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core (getText, hread, runLA, (//>), (>>>))

jarFile ∷ String
jarFile = "tika-app.jar"

java ∷ [String] → IO (ExitCode, String, String)
java args = readProcessWithExitCode "java" args ""


runTika ∷ String → IO (ExitCode, String, String)
runTika pdfFilename = do
  classpath <- lookupEnv "CLASSPATH"
  let args = ["--html", pdfFilename]
  case classpath of
    Nothing -> java ("-jar":jarFile:args)
    Just _  -> java ("org.apache.tika.cli.TikaCLI":args)


paragraphs ∷ String → [String]
paragraphs html =
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter (not ∘ isPdfMetadata) allParagraphs


isPdfMetadata ∷ String → Bool
isPdfMetadata text =
  "<<\n" `isPrefixOf` text
