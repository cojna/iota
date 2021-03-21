{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Prop.Fractional (fractionalSpec, fractionalClosedSpec) where

import Test.Prelude

fractionalSpec ::
  forall a. (Fractional a, Arbitrary a, Show a, Eq a) => Proxy a -> Spec
fractionalSpec _ = do
  describe "recip" $
    prop "x * recip x == recip x * x == 1" $ prop_mulInverse @a
  describe "^^" $ do
    prop "x ^^ (n + m) == (x ^^ n) * (x ^^ m)" $ prop_addPow @a
    prop "x ^^ (n * m) == (x ^^ n) ^^ m" $ prop_mulPow @a
    prop "x ^^ 0 == 1" $ prop_zeroPow @a

fractionalClosedSpec ::
  (Fractional a, Arbitrary a, Show a, Eq a) => (a -> Bool) -> Spec
fractionalClosedSpec validate = do
  describe "closed" $ do
    prop "recip" $ \x -> validate (recip x)

prop_mulInverse ::
  (Fractional a, Arbitrary a, Show a, Eq a) =>
  NonZero a ->
  Bool
prop_mulInverse (getNonZero -> x) = x * recip x == 1 && recip x * x == 1

{- |
 >>> 0 ^^ ((-1) + 1)
 1
 >>> (0 ^^ (-1)) * (0 ^^ 1)
 0
-}
prop_addPow ::
  (Fractional a, Eq a) =>
  NonZero a ->
  Int ->
  Int ->
  Bool
prop_addPow (getNonZero -> x) n m =
  x ^^ (n + m) == x ^^ n * x ^^ m

prop_mulPow ::
  (Fractional a, Eq a) =>
  a ->
  Int ->
  Int ->
  Bool
prop_mulPow x n m =
  x ^^ (n * m) == (x ^^ n) ^^ m

prop_zeroPow :: (Fractional a, Eq a) => a -> Bool
prop_zeroPow x = x ^^ 0 == 1
