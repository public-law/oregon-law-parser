{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy     (putStr)
import           Data.List                (isPrefixOf)
import           Data.Maybe               (fromMaybe)
import           GHC.Generics
import           Prelude.Unicode
import           System.Environment       (getArgs)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core



data Amendment =
  Amendment {
      summary ∷ String
    } deriving (Show, Generic)

instance ToJSON Amendment


main ∷ IO ()
main = do
  html ← getContents
  html
    |> newAmendment
    |> encodePretty
    |> show
    |> read
    |> putStrLn



newAmendment ∷ String → Amendment
newAmendment html =
  Amendment {
    summary = fromMaybe "(Summary is not available)" $ findSummary $ paragraphs html
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


isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence


isNotPdfMetadata ∷ String → Bool
isNotPdfMetadata text =
  not (isPdfMetadata text)


isPdfMetadata ∷ String → Bool
isPdfMetadata text =
  "<<\n" `isPrefixOf` text


-- The Railway operator
(|>) x f = f x
