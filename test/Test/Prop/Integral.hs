{-# LANGUAGE ViewPatterns #-}

module Test.Prop.Integral (integralSpec) where

import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

integralSpec :: (Arbitrary a, Integral a, Show a, Eq a) => Proxy a -> Spec
integralSpec proxy = do
    describe "divMod" $
        prop "(x `div` y) * y + (x `mod` y) == x" $ prop_divMod proxy
    describe "quotRem" $
        prop "(x `quot` y) * y + (x `rem` y) == x" $ prop_quotRem proxy

prop_divMod :: (Integral a, Arbitrary a, Show a, Eq a)
  => proxy a -> a -> NonZero a -> Bool
prop_divMod _ x (getNonZero -> y) = (x `div` y) * y + (x `mod` y) == x

prop_quotRem :: (Integral a, Arbitrary a, Show a, Eq a)
  => proxy a -> a -> NonZero a -> Bool
prop_quotRem _ x (getNonZero -> y) = (x `quot` y) * y + (x `rem` y) == x
