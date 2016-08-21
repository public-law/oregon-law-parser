{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}

module Amendment where

import           Data.Aeson        (ToJSON)
import           Data.Function     ((&))
import           Data.List         (isPrefixOf, nub, sort)
import           Data.String.Utils (split, splitWs)
import           Data.Time         (Day, defaultTimeLocale, parseTimeOrError)
import           GHC.Generics
import           StringOps
import           Text.Regex.TDFA

data Amendment =
  Amendment {
    affectedSections ∷ ChangeSet,
    bill             ∷ Bill,
    chapter          ∷ Integer,
    summary          ∷ String,
    effectiveDate    ∷ Day,
    year             ∷ Integer
  } deriving (Show, Generic)

data ChangeSet =
  ChangeSet {
    amended  ∷ [SectionNumber],
    repealed ∷ [SectionNumber]
  } deriving (Eq, Show, Generic)

type SectionNumber = String

data Bill =
  Bill {
    billType   ∷ BillType,
    billNumber ∷ Integer
  } deriving (Show, Eq, Generic)

data BillType = HB | SB
  deriving (Read, Show, Eq, Generic)


instance ToJSON Amendment
instance ToJSON Bill
instance ToJSON BillType
instance ToJSON ChangeSet



makeBill ∷ String → Bill
makeBill citation =
  let [chamber, number] = splitWs citation
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
    & splitWs -- I don't know how to capture a group yet
    & last
    & read


findChapter ∷ [String] → Integer
findChapter phrases =
  phrases
    & join
    & firstMatch "Chap. [0-9]{1,3}"
    & splitWs
    & last
    & read


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


findChangedStatutes ∷ String → ChangeSet
findChangedStatutes title =
  let clauses = split "; " title
  in ChangeSet {
    amended = sectionNumbers (head (filter (\c → "amending" `isPrefixOf` c) clauses)),
    repealed = sectionNumbers (head (filter (\c → "repealing" `isPrefixOf` c) clauses))
  }


sectionNumbers ∷ String → [String]
sectionNumbers phrase =
  -- Match ORS section numbers like 40.230 and 743A.144.
  getAllTextMatches (phrase =~ "[0-9]{1,3}[A-C]?\\.[0-9]{3}")



flatten = concat
unique = nub
