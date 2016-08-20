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

  describe "cleanUp" $ do
    it "handles extra text" $ do
      let input = "Relating to the state transient lodging tax; creating\nnew provisions; amending ORS 284.131 and\n320.305; prescribing an effective date; and pro-\nviding for revenue raising that requires approval\nby a three-fifths majority.\nWhereas Enrolled House Bill 2267 (chapter 818,"
      cleanUp input `shouldBe` "Relating to the state transient lodging tax; creating new provisions; amending ORS 284.131 and 320.305; prescribing an effective date; and providing for revenue raising that requires approval by a three-fifths majority."
