{-# LANGUAGE UnicodeSyntax #-}

module Tika where

import           Data.List         (isPrefixOf)
import           GHC.IO.Exception
import           Prelude.Unicode
import           System.Process    (readProcessWithExitCode)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core (getText, hread, runLA, (//>), (>>>))

jarFile        = "/Users/robb/lib/tika-app.jar"
javaExecutable = "java"



runTika ∷ String → IO (ExitCode, String, String)
runTika pdfFilename =
  readProcessWithExitCode javaExecutable ["-jar", jarFile, "--html", pdfFilename] ""


paragraphs ∷ String → [String]
paragraphs html =
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter (not ∘ isPdfMetadata) allParagraphs


isPdfMetadata ∷ String → Bool
isPdfMetadata text =
  "<<\n" `isPrefixOf` text
