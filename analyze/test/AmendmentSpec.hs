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
