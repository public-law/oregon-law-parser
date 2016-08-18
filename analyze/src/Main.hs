{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Amendment
import           Control.Arrow.Unicode
import           Control.Monad
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.List                (nub, sort)
import           Data.Time                (fromGregorian)
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
makeAmendment html =
  let phrases = html |> paragraphs
  in Amendment {
    summary    = phrases |> findSummary,
    citations  = phrases |> findSectionNumbers,
    bill       = html |> findCitation |> makeBill,
    year       = html |> findYear,
    effectiveDate = phrases |> findEffectiveDate
  }


findSummary ∷ [String] → String
findSummary phrases =
  case filter isSummary phrases of
    [aSummary] → cleanUp aSummary
    _          → "(Summary is not available)"


findSectionNumbers ∷ [String] → [SectionNumber]
findSectionNumbers phrases =
  phrases
    |> map sectionNumbers
    |> flatten
    |> unique
    |> sort


sectionNumbers ∷ String → [String]
sectionNumbers phrase =
  -- Match ORS section numbers like 40.230 and 743A.144.
  getAllTextMatches (phrase =~ "[0-9]{1,3}[A-C]?\\.[0-9]{3}")


flatten = concat
unique = nub
