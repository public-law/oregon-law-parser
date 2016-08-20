{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}

module Amendment where

import           Data.Aeson      (ToJSON)
import           Data.Function   ((&))
import           Data.List       (isPrefixOf, nub, sort)
import           Data.Time       (Day, defaultTimeLocale, parseTimeOrError)
import           GHC.Generics
import           StringOps
import           Text.Regex.TDFA



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



makeBill ∷ String → Bill
makeBill citation =
  let [chamber, number] = split citation
  in  Bill { billType = read chamber, billNumber = read number }


findCitation ∷ [String] → String
findCitation phrases =
  phrases
    & join
    & firstMatch "(HB|SB) [0-9]{4}"


findYear ∷ [String] → Integer
findYear input =
  input
    & join
    & firstMatch "OREGON LAWS [0-9]{4}"
    & split -- I don't know how to capture a group yet
    & last
    & convert


findEffectiveDate ∷ [String] -> Day
findEffectiveDate paragraphs =
  paragraphs
    & join
    & firstMatch "Effective date .+ [0-9]+, [0-9]{4}"
    & parseTimeOrError True defaultTimeLocale "Effective date %B %-d, %Y"


findSummary ∷ [String] → String
findSummary phrases =
  case filter isSummary phrases of
    [aSummary] → cleanUp aSummary
    _          → "(Summary is not available)"


isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence


findSectionNumbers ∷ [String] → [SectionNumber]
findSectionNumbers phrases =
  phrases
    & map sectionNumbers
    & flatten
    & unique
    & sort


sectionNumbers ∷ String → [String]
sectionNumbers phrase =
  -- Match ORS section numbers like 40.230 and 743A.144.
  getAllTextMatches (phrase =~ "[0-9]{1,3}[A-C]?\\.[0-9]{3}")




convert = read
flatten = concat
unique = nub
