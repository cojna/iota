{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Prop.Num (
  numSpec,
  additiveAbelianGroupSpec,
  numClosedSpec,
) where

import Test.Prelude

numSpec :: forall a. (Arbitrary a, Num a, Show a, Eq a) => Proxy a -> Spec
numSpec _ = do
  describe "+" $ do
    prop "x + 0 == 0 + x == x" $ prop_addUnit @a
    prop "(x + y) + z == x + (y + z)" $ prop_addAssociative @a
    prop "x + y == y + x" $ prop_addCommutative @a
  describe "-" $
    prop "x + (0 - x) == 0" $ prop_subInverse @a
  describe "*" $ do
    prop "x * 1 == 1 * x == x" $ prop_mulUnit @a
    prop "(x * y) * z == x * (y * z)" $ prop_mulAssociative @a
  describe "+, *" $ do
    prop "x * (y + z) == x * y + x * z" $ prop_leftDistributive @a
    prop "(x + y) * z == x * z + y * z" $ prop_rightDistributive @a
  describe "^" $ do
    prop "x ^ (n + m) == (x ^ n) * (x ^ m)" $ prop_addPow @a
    prop "x ^ (n * m) == (x ^ n) ^ m" $ prop_mulPow @a
    prop "x ^ 0 == 1" $ prop_zeroPow @a
  describe "abs/signum" $ do
    prop "abs x * signum x == x" $ prop_absSignum @a

additiveAbelianGroupSpec ::
  forall a. (Arbitrary a, Num a, Show a, Eq a) => Proxy a -> Spec
additiveAbelianGroupSpec _ = do
  describe "+" $ do
    prop "x + 0 == 0 + x == x" $ prop_addUnit @a
    prop "(x + y) + z == x + (y + z)" $ prop_addAssociative @a
    prop "x + y == y + x" $ prop_addCommutative @a

numClosedSpec :: (Arbitrary a, Num a, Show a, Eq a) => (a -> Bool) -> Spec
numClosedSpec validate = do
  describe "closed" $ do
    prop "+" $ \x y -> validate (x + y)
    prop "-" $ \x y -> validate (x - y)
    prop "*" $ \x y -> validate (x * y)
    prop "negate" $ \x -> validate (negate x)

prop_addUnit :: (Num a, Eq a) => a -> Bool
prop_addUnit x = x + 0 == x && 0 + x == x

prop_addAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_addAssociative x y z = (x + y) + z == x + (y + z)

prop_addCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_addCommutative x y = x + y == y + x

prop_subInverse :: (Num a, Eq a) => a -> Bool
prop_subInverse x = x - x == 0

{-# ANN prop_mulUnit "HLint: ignore Evaluate" #-}
prop_mulUnit :: (Num a, Eq a) => a -> Bool
prop_mulUnit x = x * 1 == x && 1 * x == x

prop_mulAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_mulAssociative x y z = (x * y) * z == x * (y * z)

prop_mulCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_mulCommutative x y = x * y == y * x

prop_leftDistributive :: (Num a, Eq a) => a -> a -> a -> Bool
prop_leftDistributive x y z = x * (y + z) == x * y + x * z

prop_rightDistributive :: (Num a, Eq a) => a -> a -> a -> Bool
prop_rightDistributive x y z = (x + y) * z == x * z + y * z

prop_absSignum :: (Num a, Eq a) => a -> Bool
prop_absSignum x = abs x * signum x == x

prop_addPow ::
  (Num a, Eq a) =>
  NonZero a ->
  NonNegative Int ->
  NonNegative Int ->
  Bool
prop_addPow
  (getNonZero -> x)
  (getNonNegative -> n)
  (getNonNegative -> m) =
    x ^ (n + m) == x ^ n * x ^ m

prop_mulPow ::
  (Num a, Eq a) =>
  a ->
  NonNegative Int ->
  NonNegative Int ->
  Bool
prop_mulPow
  x
  (getNonNegative -> n)
  (getNonNegative -> m) =
    x ^ (n * m) == (x ^ n) ^ m

-- need (*) commutativity
prop_powMul ::
  (Num a, Eq a) =>
  NonZero a ->
  NonZero a ->
  NonNegative Int ->
  Bool
prop_powMul
  (getNonZero -> x)
  (getNonZero -> y)
  (getNonNegative -> n) =
    x ^ n * y ^ n == (x * y) ^ n

{-# ANN prop_zeroPow "HLint: ignore Use 1" #-}
prop_zeroPow :: (Num a, Eq a) => a -> Bool
prop_zeroPow x = x ^ 0 == 1
