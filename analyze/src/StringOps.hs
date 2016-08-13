{-# LANGUAGE UnicodeSyntax #-}

module StringOps where

import           Control.Arrow.Unicode
import           Data.String.Utils     (replace, strip)



cleanUp ∷ String → String
cleanUp = fixWhitespace ⋙ fixHyphenation ⋙ strip


fixHyphenation ∷ String → String
fixHyphenation = replace "- " ""


fixWhitespace ∷ String → String
fixWhitespace = replace "\n" " "
