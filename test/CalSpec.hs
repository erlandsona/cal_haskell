module CalSpec where

import Test.Hspec
import Cal

main :: IO ()
main = hspec $ do
  describe "Cal" $ do
    describe "#zellers" $ do
      def test_init_day_saves_values
        d = Day.new(1, 1, 2012)
        assert_equal 1, d.day
        assert_equal 1, d.month
        assert_equal 2012, d.year
      end

      def test_zellers_returns_six
        d = Day.new(1, 1, 2000)
        z = d.zellers
        assert_equal 6, z
      end

      def test_zellers_returns_zero
        d = Day.new(13, 2, 2000)
        z = d.zellers
        assert_equal 0, z
      end

      def test_zellers_returns_one
        d = Day.new(1, 10, 2012)
        z = d.zellers
        assert_equal 1, z
      end

      def test_zellers_returns_two
        d = Day.new(14, 1, 1800)
        z = d.zellers
        assert_equal 2, z
      end

      def test_zellers_returns_three
        d = Day.new(2, 6, 2021)
        z = d.zellers
        assert_equal 3, z
      end

      def test_zellers_returns_four
        d = Day.new(30, 4, 2015)
        z = d.zellers
        assert_equal 4, z
      end

      def test_zellers_returns_five
        d = Day.new(27, 12, 2999)
        z = d.zellers
        assert_equal 5, z
      end

