{-# LANGUAGE ViewPatterns #-}

module Math.UtilsSpec where

import Data.Bits
import Math.Utils
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "floorSqrt" $ do
    prop "floor (sqrt x)" prop_floorSqrt
  describe "floorLog2" $ do
    it "floorLog2 0 = -1023" $ do
      floorLog2 0 `shouldBe` (-1023)
    it "floorLog2 1 = 0" $ do
      floorLog2 1 `shouldBe` 0
    it "floorLog2 2 = 1" $ do
      floorLog2 2 `shouldBe` 1
    it "floorLog2 3 = 1" $ do
      floorLog2 3 `shouldBe` 1
    it "floorLog2 1023 = 9" $ do
      floorLog2 1023 `shouldBe` 9
    it "floorLog2 1024 = 10" $ do
      floorLog2 1024 `shouldBe` 10
    it "floorLog2 1025 = 10" $ do
      floorLog2 1025 `shouldBe` 10
    prop "2 ^ n <= floorLog2 n < 2 ^ (n + 1)" prop_floorLog2

prop_floorSqrt :: NonNegative Int -> Bool
prop_floorSqrt (getNonNegative -> n) = res * res <= n && (res + 1) * (res + 1) > n
  where
    res = floorSqrt n

prop_floorLog2 :: Positive Int -> Bool
prop_floorLog2 (getPositive -> n) =
  shiftL 1 res <= n && n < shiftL 1 (res + 1)
  where
    res = floorLog2 n
