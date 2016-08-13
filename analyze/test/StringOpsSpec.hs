module StringOpsSpec where

import           StringOps
import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fixHyphenation" $ do
    it "rejoins a word" $ do
      fixHyphenation "auto- mobile" `shouldBe` "automobile"

  describe "fixWhitespace" $ do
    it "changes a newline to a space" $ do
      fixWhitespace "and\nthe story" `shouldBe` "and the story"
