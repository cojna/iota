{-# LANGUAGE BangPatterns, ViewPatterns #-}

module Math.Modulus.LogSpec (main, spec) where

import qualified Data.List    as L

import Math.Modulus ( powMod )
import Math.Modulus.Log
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "logMod" $ do
        it "logMod 3 27 998244353 = Just 3" $ do
            logMod 3 27 998244353 `shouldBe` Just 3
        it "logMod 3 123456789 998244353 = Just 772453214" $ do
            logMod 3 123456789 998244353 `shouldBe` Just 772453214
        it "logMod 1 2 1000000007 = Nothing" $ do
            logMod 1 2 1000000007 `shouldBe` Nothing
        prop "a ^ logMod a (a ^ x) == a ^ x" prop_logMod

prop_logMod :: Int -> NonNegative Int -> Prime Int -> Bool
prop_logMod a0 (getNonNegative -> x) (getPrime -> modulus)
    | Just x' <- logMod (mod a modulus) ax modulus = powMod a x' modulus == ax
    | otherwise = False
  where
    a = mod a0 modulus
    !ax = powMod a x modulus
