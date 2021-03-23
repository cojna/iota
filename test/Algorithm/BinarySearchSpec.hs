{-# LANGUAGE ViewPatterns #-}

module Algorithm.BinarySearchSpec (main, spec) where

import Algorithm.BinarySearch
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lowerBound" $ do
    prop "fixed bound for Int" prop_lowerBoundFixedBound
    prop "fixed bound for Large Int" prop_lowerBoundLargeFixedBound
    prop "fixed key for Int" prop_lowerBoundFixedKey
    prop "fixed key for Large Int" prop_lowerBoundLargeFixedKey
    it "lowerBound 0 1 (0<=) = 0" $ do
      lowerBound 0 1 (0 <=) `shouldBe` 0
    it "lowerBound 0 1 (1<=) = 1" $ do
      lowerBound 0 1 (1 <=) `shouldBe` 1
    it "lowerBound 0 1 ((-1)<=) = 0" $ do
      lowerBound 0 1 ((-1) <=) `shouldBe` 0
    it "lowerBound 0 0 (0<=)" $ do
      lowerBound 0 0 (0 <=) `shouldBe` 0
  describe "upperBound" $ do
    prop "fixed bound for Int" prop_upperBoundFixedBound
    prop "fixed bound for Large Int" prop_upperBoundLargeFixedBound
    prop "fixed key for Int" prop_upperBoundFixedKey
    prop "fixed key for Large Int" prop_upperBoundLargeFixedKey
    it "upperBound 0 1 (<=0) = 0" $ do
      upperBound 0 1 (<= 0) `shouldBe` 0
    it "upperBound 0 1 (<=1) = 1" $ do
      upperBound 0 1 (<= 1) `shouldBe` 1
    it "upperBound 0 1 (<=2) = 1" $ do
      upperBound 0 1 (<= 2) `shouldBe` 1
    it "upperBound 0 0 (<=0) = 0" $ do
      upperBound 0 0 (<= 0) `shouldBe` 0

prop_lowerBoundFixedBound :: Int -> Bool
prop_lowerBoundFixedBound k = k == lowerBound minBound maxBound (k <=)

prop_upperBoundFixedBound :: Int -> Bool
prop_upperBoundFixedBound k = k == upperBound minBound maxBound (<= k)

prop_lowerBoundLargeFixedBound :: Large Int -> Bool
prop_lowerBoundLargeFixedBound (getLarge -> k) =
  k == lowerBound minBound maxBound (k <=)

prop_upperBoundLargeFixedBound :: Large Int -> Bool
prop_upperBoundLargeFixedBound (getLarge -> k) =
  k == upperBound minBound maxBound (<= k)

prop_lowerBoundFixedKey :: Negative Int -> Positive Int -> Bool
prop_lowerBoundFixedKey (getNegative -> lb) (getPositive -> ub) =
  lowerBound lb ub (0 <=) == 0

prop_upperBoundFixedKey :: Negative Int -> Positive Int -> Bool
prop_upperBoundFixedKey (getNegative -> lb) (getPositive -> ub) =
  upperBound lb ub (<= 0) == 0

prop_lowerBoundLargeFixedKey :: Negative (Large Int) -> Positive (Large Int) -> Bool
prop_lowerBoundLargeFixedKey (getLarge . getNegative -> lb) (getLarge . getPositive -> ub) =
  lowerBound lb ub (0 <=) == 0

prop_upperBoundLargeFixedKey :: Negative (Large Int) -> Positive (Large Int) -> Bool
prop_upperBoundLargeFixedKey (getLarge . getNegative -> lb) (getLarge . getPositive -> ub) =
  upperBound lb ub (<= 0) == 0
