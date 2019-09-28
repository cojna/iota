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
    describe "powMod:" $
        prop "same to naive" prop_powModSameToNaive
    describe "recipMod:" $
        prop "x * recip x == 1" prop_recipMod
    describe "logMod:" $ do
        prop "a ^ logMod a (a ^ x) == a ^ x" prop_logMod

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
    = x `mod` m == 0 || x * recipMod x m `mod` m == 1

prop_logMod :: Int -> NonNegative Int -> Prime Int -> Bool
prop_logMod a0 (getNonNegative -> x) (getPrime -> modulus)
    | Just x' <- logMod (mod a modulus) ax modulus = powMod a x' modulus == ax
    | otherwise = False
  where
    a = mod a0 modulus
    !ax = powMod a x modulus
