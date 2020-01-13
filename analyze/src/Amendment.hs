{-# LANGUAGE DeriveGeneric #-}

module Amendment where

import           Data.Aeson        (ToJSON)
import           Data.Function     ((&))
import           Data.List         (isPrefixOf, isSubsequenceOf, nub, sort)
import           Data.String.Utils (split, splitWs)
import           Data.Time         (Day, defaultTimeLocale, parseTimeOrError)
import           GHC.Generics
import           Prelude.Unicode
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
  let maybeMatch = phrases
        & join
        & firstMatch "(HB|SB) [0-9]+"
  in case(maybeMatch) of
    Just s -> s
    Nothing -> error ("Could not find a citation")


findYear ∷ [String] → Integer
findYear input =
  let maybeMatch = input
        & join
        & firstMatch "OREGON LAWS [0-9]{4}"

  in case(maybeMatch) of
    Just s -> s
              & splitWs -- I don't know how to capture a group yet
              & last
              & read
    Nothing -> error ("Could not find the year")


findChapter ∷ [String] → Integer
findChapter phrases =
  let maybeMatch =
        phrases
        & join
        & firstMatch "Chap. [0-9]{1,3}"

  in case(maybeMatch) of
    Just s -> s
              & splitWs
              & last
              & read
    Nothing -> error ("Could not find the Chapter")


findEffectiveDate ∷ [String] → Day
findEffectiveDate paragraphs =
  let maybeMatch =
        paragraphs
        & join
        & firstMatch "Effective date .+ [0-9]+, [0-9]{4}"
  in case (maybeMatch) of
    Just s -> parseTimeOrError True defaultTimeLocale "Effective date %B %-d, %Y" s
    Nothing -> error ("Could not find the Effective Date")


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
      extractFrom section = findSectionNumbers ∘ filter (\c → section `isSubsequenceOf` c) $ clauses
  in ChangeSet {
    amended  = extractFrom "amending",
    repealed = extractFrom "repealing"
  }


sectionNumbers ∷ String → [String]
sectionNumbers phrase =
  -- Match ORS section numbers like 40.230 and 743A.144.
  getAllTextMatches (phrase =~ "[0-9]{1,3}[A-C]?\\.[0-9]{3}")



flatten = concat
unique = nub
