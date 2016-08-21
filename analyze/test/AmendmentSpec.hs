module AmendmentSpec where

import           Amendment
import           Data.Time  (fromGregorian)
import           Test.Hspec



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isSummary" $ do
    it "is true for a valid summary" $ do
      let summary_hb_4047 = "Relating to speed limits on highways that traverse state lines; creating new provisions; amending ORS 811.111; and declaring an emergency."
      isSummary summary_hb_4047 `shouldBe` True


  describe "makeBill" $ do
    it "can parse a typical house bill" $ do
      makeBill "HB 4047" `shouldBe` Bill { billType = HB, billNumber = 4047 }

    it "can parse a typical senate bill" $ do
      makeBill "SB 1532" `shouldBe` Bill { billType = SB, billNumber = 1532 }


  describe "findCitation" $ do
    it "can find it in an HB title" $ do
      findCitation ["AN ACT HB 4047"] `shouldBe` "HB 4047"

    it "can find it in an SB title" $ do
      findCitation ["AN ACT SB 1234"] `shouldBe` "SB 1234"

  describe "findYear" $ do
    it "returns just the year" $ do
      findYear ["OREGON LAWS 2016", "Some junk"] `shouldBe` 2016

  describe "findChapter" $ do
    it "can find it" $ do
      findChapter ["Chap. 102"] `shouldBe` 102

  describe "findEffectiveDate" $ do
    it "picks out the right one" $ do
      let ps = ["Nope.", "Approved by the Governor March 3, 2016 Filed in the office of Secretary of State March 3, 2016 Effective date January 17, 2017"]
      (findEffectiveDate ps) `shouldBe` (fromGregorian 2017 1 17)

  describe "findChangedStatutes" $ do
    it "picks out the amended and repealed correctly" $ do
      let title = "Relating to student safety; creating new provisions; amending ORS 165.570 and sections 1 and 2, chapter 93, Oregon Laws 2014; repealing ORS 180.650 and 180.660; and declaring an emergency."
      findChangedStatutes title `shouldBe` ChangeSet { amended = ["165.570"], repealed = ["180.650", "180.660"] }
