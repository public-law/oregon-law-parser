{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}

module Amendment where

import           Data.Aeson        (ToJSON)
import           Data.List         (isPrefixOf)
import           Data.String.Utils (splitWs)
import           Data.Time         (Day)
import           GHC.Generics
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
      chapter       ∷ Integer,
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
  let [chamber, number] = splitWs citation
  in  Bill { billType = read chamber, billNumber = read number }


findCitation ∷ String → String
findCitation input =
  input =~ "(HB|SB) [0-9]{4}"
    |> getAllTextMatches
    |> head


findYear ∷ String → Integer
findYear input =
  input =~ "OREGON LAWS [0-9]{4}"
    |> getAllTextMatches
    |> head
    |> splitWs
    |> last
    |> read


--
-- The Railway operator
--
(|>) ∷ t1 -> (t1 -> t2) -> t2
(|>) x f = f x
