module CalSpec where

import Test.Hspec
import Cal

main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "Cal" $ do
    describe "#zellers" $ do
      it "returns 6" $
        zellers 1 12 2000 `shouldBe` 6

--       it "returns 0" $ do
--         zellers 13 2 2000 `shouldBe` 0

--       it "returns one" $ do
--         zellers 1 10 2012 `shouldBe`  1

--       it "returns two" $ do
--         zellers 14 1 1800 `shouldBe`  2

--       it "returns three" $ do
--         zellers 2 6 2021 `shouldBe`  3

--       it "returns four" $ do
--         zellers 30 4 2015 `shouldBe`  4

--       it "returns five" $ do
--         zellers 27 12 2999 `shouldBe`  5
