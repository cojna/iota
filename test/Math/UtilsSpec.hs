{-# LANGUAGE ViewPatterns #-}

module Math.UtilsSpec where

import Math.Utils
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "floorSqrt" $ do
    it "floorSqrt 0 = 0" $ do
      floorSqrt 0 `shouldBe` 0
    it "floorSqrt 1 = 1" $ do
      floorSqrt 1 `shouldBe` 1
    it "floorSqrt (2^52+2^27) = 67108864" $ do
      floorSqrt (2 ^ (52 :: Int) + 2 ^ (27 :: Int))
        `shouldBe` 67108864
    it "floorSqrt maxBound = 3037000499" $ do
      floorSqrt maxBound `shouldBe` 3037000499
    prop "floor (sqrt x)" prop_floorSqrt
  describe "integerFloorSqrt" $ do
    it "integerFloorSqrt 0 = 0" $ do
      integerFloorSqrt 0 `shouldBe` 0
    it "integerFloorSqrt 1 = 1" $ do
      integerFloorSqrt 1 `shouldBe` 1
    it "integerFloorSqrt 2 = 1" $ do
      integerFloorSqrt 2 `shouldBe` 1
    it "integerFloorSqrt 4 = 2" $ do
      integerFloorSqrt 4 `shouldBe` 2
    prop "integerFloorSqrt == floorSqrt" $ \n ->
      fromIntegral (integerFloorSqrt (fromIntegral n))
        == floorSqrt n

prop_floorSqrt :: NonNegative Int -> Bool
prop_floorSqrt (getNonNegative -> n) =
  res * res <= fromIntegral n
    && fromIntegral n < (res + 1) * (res + 1)
  where
    res = toInteger $ floorSqrt n
