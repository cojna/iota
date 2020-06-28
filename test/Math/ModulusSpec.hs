{-# LANGUAGE BangPatterns, ViewPatterns #-}

module Math.ModulusSpec (main, spec) where

import           Data.Int
import           Data.IntMod
import qualified Data.List    as L
import           Data.Word
import           Math.Modulus
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "powMod" $ do
        it "powMod 2 0 1000000007 = 1" $ do
            powMod 2 0 1000000007 `shouldBe` 1
        it "powMod 0 0 1000000007 = 1" $ do
            powMod 0 0 1000000007 `shouldBe` 1
        it "powMod 2 1000000005 1000000007 = 500000004" $ do
            powMod 2 1000000005 1000000007 `shouldBe` 500000004
        it "powMod 2 (-1) 1000000007 = 500000004" $ do
            powMod 2 (-1) 1000000007 `shouldBe` 500000004
        it "powMod 123456789 998244353 998244353 = 123456789" $ do
            powMod 123456789 998244353 998244353 `shouldBe` 123456789
        it "powMod (-2) 2 1000000007 = 4" $ do
            powMod (-2) 2 1000000007 `shouldBe` 4
        it "powMod (-2) 3 1000000007 = 999999999" $ do
            powMod (-2) 3 1000000007 `shouldBe` 999999999
        prop "same to naive" prop_powModSameToNaive
        prop "Fermat's little theorem" prop_Fermat'sLittleTheorem
    describe "recipMod" $ do
        it "recipMod 2 1000000007 = 500000004" $ do
            recipMod 2 1000000007 `shouldBe` 500000004
        it "recipMod 10 1000000007 = 700000005" $ do
            recipMod 10 1000000007 `shouldBe` 700000005
        prop "x * recip x == 1" prop_recipMod
    describe "logMod" $ do
        it "logMod 3 27 998244353 = Just 3" $ do
            logMod 3 27 998244353 `shouldBe` Just 3
        it "logMod 3 123456789 998244353 = Just 772453214" $ do
            logMod 3 123456789 998244353 `shouldBe` Just 772453214
        it "logMod 1 2 1000000007 = Nothing" $ do
            logMod 1 2 1000000007 `shouldBe` Nothing
        prop "a ^ logMod a (a ^ x) == a ^ x" prop_logMod

naivePowMod :: Integer -> Int -> Integer -> Integer
naivePowMod x n m = L.foldl' (\acc x -> acc * x `mod` m) 1 $ replicate n x

prop_powModSameToNaive :: NonNegative Integer -> Small Word -> Prime Integer -> Bool
prop_powModSameToNaive
    (getNonNegative -> x)
    (fromIntegral.getSmall -> n)
    (getPrime -> m)
    = powMod x n m == naivePowMod x n m

prop_Fermat'sLittleTheorem :: NonNegative Int -> Prime Int -> Bool
prop_Fermat'sLittleTheorem
    (getNonNegative -> x) (getPrime -> p)
    = powMod x p p == x

prop_recipMod :: Positive Integer -> Prime Integer -> Bool
prop_recipMod
    (getPositive -> x)
    (getPrime -> m)
    = x `mod` m == 0 || x * recipMod x m `mod` m == 1

prop_logMod :: Int -> NonNegative Int -> Prime Int -> Bool
prop_logMod a0 (getNonNegative -> x) (getPrime -> modulus)
    | Just x' <- logMod (mod a modulus) ax modulus = powMod a x' modulus == ax
    | otherwise = False
  where
    a = mod a0 modulus
    !ax = powMod a x modulus
