{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Prop.Integral (integralSpec) where

import Test.Prelude

integralSpec ::
  forall a. (Arbitrary a, Integral a, Show a, Eq a) => Proxy a -> Spec
integralSpec proxy = do
  describe "divMod" $
    prop "(x `div` y) * y + (x `mod` y) == x" $ prop_divMod @a
  describe "quotRem" $
    prop "(x `quot` y) * y + (x `rem` y) == x" $ prop_quotRem @a

prop_divMod ::
  (Integral a, Arbitrary a, Show a, Eq a) =>
  a ->
  NonZero a ->
  Bool
prop_divMod x (getNonZero -> y) = (x `div` y) * y + (x `mod` y) == x

prop_quotRem ::
  (Integral a, Arbitrary a, Show a, Eq a) =>
  a ->
  NonZero a ->
  Bool
prop_quotRem x (getNonZero -> y) = (x `quot` y) * y + (x `rem` y) == x
