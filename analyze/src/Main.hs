{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Amendment
import           Control.Arrow.Unicode
import           Control.Monad
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           GHC.IO.Exception
import           Prelude.Unicode
import           StringOps
import           System.Environment       (getArgs)
import           Text.Regex.TDFA
import           Tika



main ∷ IO ()
main = do
  args ← getArgs
  when (length args /= 1) $
    fail "Usage: analyze [filename]"

  let pdfFilename = head args
  (errCode, rawHTML, stderr') ← runTika pdfFilename
  when (errCode /= ExitSuccess) $
    fail stderr'

  rawHTML
    |> tikaOutputToJson
    |> B.putStr


tikaOutputToJson ∷ String → B.ByteString
tikaOutputToJson html =
  html |> makeAmendment |> encodePretty


makeAmendment ∷ String → Amendment
makeAmendment tikaHtmlOutput =
  let phrases = tikaHtmlOutput |> paragraphs
  in Amendment {
    bill          = phrases |> findCitation |> makeBill,
    summary       = phrases |> findSummary,
    citations     = phrases |> findSectionNumbers,
    year          = phrases |> findYear,
    effectiveDate = phrases |> findEffectiveDate
  }
