{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}

module Amendment where

import           Control.Arrow.Unicode
import           Data.Aeson            (ToJSON)
import           Data.List             (isPrefixOf)
import           Data.Time             (Day, defaultTimeLocale,
                                        parseTimeOrError)
import           GHC.Generics
import           StringOps

type SectionNumber = String

data BillType = HB | SB
  deriving (Read, Show, Eq, Generic)

data Bill =
  Bill {
    billType   ∷ BillType,
    billNumber ∷ Integer
  } deriving (Show, Eq, Generic)

data Amendment =
  Amendment {
      summary       ∷ String,
      citations     ∷ [SectionNumber],
      year          ∷ Integer,
      bill          ∷ Bill,
      effectiveDate ∷ Day
    } deriving (Show, Generic)

instance ToJSON Amendment
instance ToJSON BillType
instance ToJSON Bill



isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence


makeBill ∷ String -> Bill
makeBill citation =
  let [chamber, number] = split citation
  in  Bill { billType = read chamber, billNumber = read number }


findCitation ∷ String → String
findCitation input =
  firstMatch "(HB|SB) [0-9]{4}" input


findYear ∷ String → Integer
findYear input =
  input
    |> firstMatch "OREGON LAWS [0-9]{4}"
    |> split -- I don't know how to capture a group yet
    |> last
    |> convert


findEffectiveDate ∷ [String] -> Day
findEffectiveDate paragraphs =
  paragraphs
    |> join
    |> firstMatch "Effective date .+ [0-9]+, [0-9]{4}"
    |> parseTimeOrError True defaultTimeLocale "Effective date %B %-d, %Y"


convert = read

--
-- The Railway operator
--
(|>) ∷ t1 -> (t1 -> t2) -> t2
(|>) x f = f x
