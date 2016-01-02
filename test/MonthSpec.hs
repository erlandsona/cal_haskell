-- file MathSpec.hs
module MonthSpec where

import Test.Hspec
import Month

main :: IO ()
main = hspec $ do
  describe "Month" $ do
    it "should print the result of 3 * 4" $
      threeTimesFour `shouldBe` 12

    it "test_init_month_saves_values" $
      pendingWith "test_init_month_saves_values"

    it "test_to_s_on_jan_2012" $
      pendingWith "test_to_s_on_jan_2012"

    it "test_to_s_on_jan_2017" $
      pendingWith "test_to_s_on_jan_2017"

    it "test_to_s_on_may_2016" $
      pendingWith "test_to_s_on_may_2016"

    it "test_to_s_on_jan_2000" $
      pendingWith "test_to_s_on_jan_2000"

    it "test_to_s_on_may_2000" $
      pendingWith "test_to_s_on_may_2000"

    it "test_to_s_on_feb_2000" $
      pendingWith "test_to_s_on_feb_2000"

    it "test_to_s_on_march_2000" $
      pendingWith "test_to_s_on_march_2000"

    it "test_to_s_on_jun_2000" $
      pendingWith "test_to_s_on_jun_2000"

    it "test_to_s_on_sep_2000" $
      pendingWith "test_to_s_on_sep_2000"

    it "test_to_s_on_feb_2015" $
      pendingWith "test_to_s_on_feb_2015"
