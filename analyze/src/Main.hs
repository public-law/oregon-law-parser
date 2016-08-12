{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Control.Arrow.Unicode
import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as ByteString
import           Data.List                (isPrefixOf, nub, sort)
import           Data.Maybe               (fromMaybe)
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


main = do
  args ← getArgs
  let pdfFilename = head args

  (errCode, stdout', stderr') <- readProcessWithExitCode "java" ["-jar", "/Users/robb/lib/tika-app.jar", "--html", pdfFilename] ""
  -- putStrLn $ "stderr: " ++ stderr'
  -- putStrLn $ "errCode: " ++ show errCode

  let html = stdout'
  html
    |> makeAmendment
    |> encodePretty
    |> ByteString.putStr


makeAmendment ∷ String → Amendment
makeAmendment html =
  let phrases = paragraphs html
  in Amendment {
    summary   = phrases |> findSummary |> fromMaybe "(Summary is not available)",
    citations = phrases |> allSectionNumbers
  }


paragraphs ∷ String → [String]
paragraphs html =
  -- TODO: Switch to TagSoup for the HTML parsing
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter isNotPdfMetadata allParagraphs


findSummary ∷ [String] → Maybe String
findSummary phrases =
  case filter isSummary phrases of
    [aSummary] → Just (cleanUp aSummary)
    _          → Nothing


allSectionNumbers ∷ [String] → [SectionNumber]
allSectionNumbers phrases =
  phrases
    |> map sectionNumbers
    |> concat
    |> nub
    |> sort


sectionNumbers ∷ String → [String]
sectionNumbers text =
  getAllTextMatches $ text =~ ("[0-9]{1,3}\\.[0-9]{3}" ∷ String)


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
