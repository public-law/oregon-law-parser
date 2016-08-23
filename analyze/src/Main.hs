{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Amendment
import           Control.Monad
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Eq.Unicode
import           Data.Function            ((&))
import           GHC.IO.Exception
import           System.Environment       (getArgs)
import           Tika


main ∷ IO ()
main = do
  args ← getArgs
  when (length args ≠ 1) $
    fail "Usage: analyze [filename]"

  let pdfFilename = head args
  (errCode, rawHTML, stderr') ← runTika pdfFilename
  when (errCode ≠ ExitSuccess) $
    fail stderr'

  B.putStr $ tikaOutputToJson rawHTML


tikaOutputToJson ∷ String → B.ByteString
tikaOutputToJson html =
  html
    |> paragraphs
    |> makeAmendment
    |> encodePretty


makeAmendment ∷ [String] → Amendment
makeAmendment phrases =
  Amendment {
    bill             = phrases |> findCitation |> makeBill,
    summary          = phrases |> findSummary,
    affectedSections = phrases |> findSummary |> findChangedStatutes,
    year             = phrases |> findYear,
    effectiveDate    = phrases |> findEffectiveDate,
    chapter          = phrases |> findChapter
  }

-- Function application operator from Elm, F#, and Elixir
(|>) = (&)
