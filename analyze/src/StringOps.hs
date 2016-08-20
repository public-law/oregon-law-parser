{-# LANGUAGE UnicodeSyntax #-}

module StringOps where

import           Control.Arrow.Unicode
import           Data.List             (isSuffixOf)
import           Data.String.Utils     (replace, split, strip)
import           Text.Regex.TDFA



cleanUp ∷ String → String
cleanUp = fixWhitespace ⋙ fixHyphenation ⋙ strip ⋙ splitIntoSentences ⋙ first


fixWhitespace ∷ String → String
fixWhitespace = replace "\n" " "


fixHyphenation ∷ String → String
fixHyphenation = replace "- " ""


splitIntoSentences ∷ String → [String]
splitIntoSentences input =
  fmap (\s → if "." `isSuffixOf` s then s else s ++ ".") (split ". " input)


firstMatch ∷ String → String → String
firstMatch regex input =
  getFirstMatch (input =~ regex)


getFirstMatch = getAllTextMatches ⋙ first


--
-- More-conventional function names
--
first = head
join = unwords
