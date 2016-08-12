{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Arrow.Unicode
import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.List                (isPrefixOf, nub, sort)
import qualified Data.Text                as Text
import           GHC.Generics
import           Prelude.Unicode
import           System.Environment       (getArgs)
import           System.Process           (readProcessWithExitCode)
import           Text.HandsomeSoup
import           Text.Regex.Posix
import           Text.XML.HXT.Core


type SectionNumber = String

data Amendment =
  Amendment {
      summary   ∷ String,
      citations ∷ [SectionNumber]
    } deriving (Show, Generic)

instance ToJSON Amendment


main :: IO ()
main = do
  args ← getArgs
  let pdfFilename = head args

  (errCode, rawHTML, stderr') <- readProcessWithExitCode "java" ["-jar", "/Users/robb/lib/tika-app.jar", "--html", pdfFilename] ""
  -- putStrLn $ "stderr: " ++ stderr'
  -- putStrLn $ "errCode: " ++ show errCode

  rawHTML
    |> htmlToJson
    |> B.putStr


htmlToJson ∷ String → B.ByteString
htmlToJson = makeAmendment ⋙ encodePretty


makeAmendment ∷ String → Amendment
makeAmendment html =
  let phrases = html |> paragraphs
  in Amendment {
    summary   = phrases |> findSummary,
    citations = phrases |> findSectionNumbers
  }


paragraphs ∷ String → [String]
paragraphs html =
  -- TODO: Switch to TagSoup for the HTML parsing
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter isNotPdfMetadata allParagraphs


findSummary ∷ [String] → String
findSummary phrases =
  case filter isSummary phrases of
    [aSummary] → cleanUp aSummary
    _          → "(Summary is not available)"


findSectionNumbers ∷ [String] → [SectionNumber]
findSectionNumbers phrases =
  phrases
    |> map sectionNumbers
    |> concat
    |> nub
    |> sort


sectionNumbers ∷ String → [String]
sectionNumbers phrase =
  -- Match ORS section numbers like 40.230 and 743A.144.
  getAllTextMatches (phrase =~ "[0-9]{1,3}[A-C]?\\.[0-9]{3}")


isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence


isNotPdfMetadata ∷ String → Bool
isNotPdfMetadata text =
  not ("<<\n" `isPrefixOf` text)


cleanUp ∷ String → String
cleanUp = tr '\n' ' ' ⋙ strip


--
-- String functions
--

tr ∷ Char → Char → String → String
tr old new =
  map (\c → if c == old then new; else c)


strip ∷ String → String
strip = Text.pack ⋙ Text.strip ⋙ Text.unpack


-- The Railway operator
(|>) x f = f x
