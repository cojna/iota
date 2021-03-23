{-# LANGUAGE ViewPatterns #-}

module Math.Modulus.SqrtSpec (main, spec) where

import Math.Modulus.Sqrt
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sqrtMod" $ do
    it "sqrtMod 2 1000000007 = [59713600,940286407]" $ do
      sqrtMod 2 1000000007 `shouldBe` [59713600, 940286407]
    it "sqrtMod 3 1000000007 = [82062379,917937628]" $ do
      sqrtMod 3 1000000007 `shouldBe` [82062379, 917937628]
    it "sqrtMod 4 1000000007 = [2,1000000005]" $ do
      sqrtMod 4 1000000007 `shouldBe` [2, 1000000005]
    it "sqrtMod 5 1000000007 = []" $ do
      sqrtMod 5 1000000007 `shouldBe` []
    it "sqrtMod 2 998244353 = [116195171,882049182]" $ do
      sqrtMod 2 998244353 `shouldBe` [116195171, 882049182]
    it "sqrtMod 3 998244353 = []" $ do
      sqrtMod 3 998244353 `shouldBe` []
    it "sqrtMod 4 998244353 = [2,998244351]" $ do
      sqrtMod 4 998244353 `shouldBe` [2, 998244351]
    it "sqrtMod 5 998244353 = []" $ do
      sqrtMod 5 998244353 `shouldBe` []
    prop "(sqrtMod a p) ^ 2 == a" prop_sqrtMod

prop_sqrtMod :: Int -> Prime Int -> Bool
prop_sqrtMod a (getPrime -> p) =
  and [res * res `rem` p == mod a p | res <- sqrtMod a p]
