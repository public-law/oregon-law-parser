module AmendmentSpec where

import           Amendment
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
      findCitation "AN ACT HB 4047" `shouldBe` Just "HB 4047"

    it "can find it in an SB title" $ do
      findCitation "AN ACT SB 1234" `shouldBe` Just "SB 1234"

    it "fails gracefully when not found" $ do
      findCitation "Eff you!" `shouldBe` Nothing
