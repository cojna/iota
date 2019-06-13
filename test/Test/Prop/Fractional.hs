{-# LANGUAGE ViewPatterns #-}

module Test.Prop.Fractional (fractionalSpec, fractionalClosedSpec) where

import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

fractionalSpec :: (Fractional a, Arbitrary a, Show a, Eq a) => Proxy a -> Spec
fractionalSpec proxy = do
    describe "recip" $
        prop "x * recip x == recip x * x == 1" $ prop_mulInverse proxy
    describe "^^" $ do
        prop "x ^^ (n + m) == (x ^^ n) * (x ^^ m)" $ prop_addPow proxy
        prop "x ^^ (n * m) == (x ^^ n) ^^ m" $ prop_mulPow proxy
        prop "x ^^ 0 == 1" $ prop_zeroPow proxy

fractionalClosedSpec :: (Fractional a, Arbitrary a, Show a, Eq a) => (a -> Bool) -> Spec
fractionalClosedSpec validate = do
  describe "closed" $ do
    prop "recip" $ \x -> validate (recip x)

prop_mulInverse :: (Fractional a, Arbitrary a, Show a, Eq a)
  => proxy a -> NonZero a -> Bool
prop_mulInverse _ (getNonZero -> x) = x * recip x == 1 && recip x * x == 1

-- |
-- >>> 0 ^^ ((-1) + 1)
-- 1
-- >>> (0 ^^ (-1)) * (0 ^^ 1)
-- 0
prop_addPow
    :: (Fractional a, Eq a)
    => Proxy a -> NonZero a -> Int -> Int -> Bool
prop_addPow _ (getNonZero -> x) n m
    = x ^^ (n + m) == x ^^ n * x ^^ m

prop_mulPow
    :: (Fractional a, Eq a)
    => Proxy a -> a -> Int -> Int -> Bool
prop_mulPow _ x n m
    = x ^^ (n * m) == (x ^^ n) ^^ m

prop_zeroPow :: (Fractional a, Eq a) => Proxy a -> a -> Bool
prop_zeroPow _ x = x ^^ 0 == 1
