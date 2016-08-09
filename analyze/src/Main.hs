{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.List                (isPrefixOf, nub, sort)
import           Data.Maybe               (fromMaybe)
import           GHC.Generics
import           Prelude.Unicode
import           System.Environment       (getArgs)
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


main ∷ IO ()
main = do
  html ← getContents
  html
    |> makeAmendment
    |> encodePretty
    |> show
    |> read
    |> putStrLn


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
    [x] → Just x
    _   → Nothing


allSectionNumbers ∷ [String] → [SectionNumber]
allSectionNumbers phrases =
  phrases
    |> (map sectionNumbers)
    |> concat
    |> nub
    |> sort


sectionNumbers ∷ String → [SectionNumber]
sectionNumbers text =
  let pattern = "[0-9]{1,3}\\.[0-9]{3}" ∷ String
  in getAllTextMatches $ text =~ pattern


isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence


isNotPdfMetadata ∷ String → Bool
isNotPdfMetadata text =
  not ("<<\n" `isPrefixOf` text)


-- The Railway operator
(|>) x f = f x
