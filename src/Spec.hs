-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- Would love to figure out why hspec-discover
-- doesn't work in this case but I feel it's
-- more valuable to keep specs in the same directory
-- as the source to keep track of which source files
-- don't have specs written.

import Test.Hspec
import Test.QuickCheck

import qualified Day.Spec   as DS
import qualified Month.Spec as MS
import qualified Year.Spec  as YS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Maths" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Day"    DS.spec
  describe "Month"  MS.spec
  describe "Year"   YS.spec
