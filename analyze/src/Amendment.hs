{-# LANGUAGE UnicodeSyntax #-}

module Amendment where

import           Data.List (isPrefixOf)


isSummary ∷ String → Bool
isSummary sentence =
  "Relating to" `isPrefixOf` sentence
