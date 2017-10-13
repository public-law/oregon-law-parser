{-# LANGUAGE RecordWildCards #-}

module Tika where

import           Cli
import           Data.List         (isPrefixOf)
import           GHC.IO.Exception
import           Prelude.Unicode
import           System.Process    (readProcessWithExitCode)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core (getText, hread, runLA, (//>), (>>>))


runTika ∷ Options → IO (ExitCode, String, String)
runTika Options{..} =
  readProcessWithExitCode
    javaExecutable
    [ "-jar", tikaJarPath
    , "--html", inputFilePath
    ]
    ""


paragraphs ∷ String → [String]
paragraphs html =
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter (not ∘ isPdfMetadata) allParagraphs


isPdfMetadata ∷ String → Bool
isPdfMetadata text =
  "<<\n" `isPrefixOf` text
