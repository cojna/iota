{-# LANGUAGE ViewPatterns #-}

module Math.ModulusSpec where

import           Data.Int
import qualified Data.List                 as L
import           Data.Word
import           Math.Modulus
import           Math.Prime.Arbitrary
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec = do
    describe "powMod" $
        prop "same to naive" prop_powModSameToNaive
    describe "recipMod" $
        prop "x * recip x == 1" prop_recipMod

naivePowMod :: Integer -> Int -> Integer -> Integer
naivePowMod x n m = L.foldl' (\acc x -> acc * x `mod` m) 1 $ replicate n x

prop_powModSameToNaive :: NonNegative Integer -> Small Word -> Prime Integer -> Bool
prop_powModSameToNaive
    (getNonNegative -> x)
    (fromIntegral.getSmall -> n)
    (getPrime -> m)
    = powMod x n m == naivePowMod x n m

prop_recipMod :: Positive Integer -> Prime Integer -> Bool
prop_recipMod
    (getPositive -> x)
    (getPrime -> m)
    = x * recipMod x m `mod` m == 1

