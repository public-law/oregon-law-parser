module Main where

import           Amendment
import           Control.Arrow.Unicode
import           Control.Monad
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Eq.Unicode
import           Data.Function            ((&))
import           GHC.IO.Exception
import           Cli
import           Tika


main ∷ IO ()
main = do
  (errCode, rawHTML, stderr') ← runTika =<< getOptions
  when (errCode ≠ ExitSuccess)
    (fail stderr')

  B.putStr (tikaOutputToJson rawHTML)


tikaOutputToJson ∷ String → B.ByteString
tikaOutputToJson = paragraphs ⋙ makeAmendment ⋙ encodePretty


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
