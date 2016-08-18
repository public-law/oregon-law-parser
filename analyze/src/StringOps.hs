{-# LANGUAGE UnicodeSyntax #-}

module StringOps where

import           Control.Arrow.Unicode
import           Data.List             (intercalate, isPrefixOf)
import           Data.String.Utils     (replace, splitWs, strip)
import           Text.Regex.TDFA



cleanUp ∷ String → String
cleanUp = fixWhitespace ⋙ fixHyphenation ⋙ strip


fixHyphenation ∷ String → String
fixHyphenation = replace "- " ""


fixWhitespace ∷ String → String
fixWhitespace = replace "\n" " "


firstMatch ∷ String -> String -> String
firstMatch regex input =
  getFirstMatch (input =~ regex)


getFirstMatch = getAllTextMatches ⋙ first


--
-- More-conventional function names
--
first = head
split = splitWs
join = unwords
