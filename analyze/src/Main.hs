{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy     (putStr)
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
  let text = paragraphs html
  in Amendment {
    summary   = text |> findSummary |> fromMaybe "(Summary is not available)",
    citations = text |> allSectionNumbers
  }


paragraphs ∷ String → [String]
paragraphs html =
  let allParagraphs = runLA (hread >>> css "p" //> getText) html
  in filter isNotPdfMetadata allParagraphs


findSummary ∷ [String] → Maybe String
findSummary text =
  case filter isSummary text of
    [x] → Just x
    _   → Nothing


allSectionNumbers ∷ [String] → [SectionNumber]
allSectionNumbers lines =
  lines
    |> (map sectionNumbers)
    |> concat
    |> nub
    |> sort


sectionNumbers ∷ String → [SectionNumber]
sectionNumbers text =
  let pattern = "[0-9]{1,3}\\.[0-9]{3}" ∷ String
  in getAllTextMatches $ text =~ pattern ∷ [SectionNumber]


isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence


isNotPdfMetadata ∷ String → Bool
isNotPdfMetadata text =
  not ("<<\n" `isPrefixOf` text)


-- The Railway operator
(|>) x f = f x
