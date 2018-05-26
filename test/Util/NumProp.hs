{-# LANGUAGE ViewPatterns #-}

module Util.NumProp where

import           Data.Proxy
import           Data.Proxy.Util
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary


numSpec :: (Arbitrary a, Num a, Show a, Eq a) => Proxy a -> Spec
numSpec proxy = do
    describe "+" $ do
        prop "x + 0 == 0 + x == x" $ lift1 proxy prop_addUnit
        prop "(x + y) + z == x + (y + z)" $ lift3 proxy prop_addAssociative
        prop "x + y == y + x" $ lift2 proxy prop_addCommutative
    describe "-" $
        prop "x + (0 - x) == 0" $ lift1 proxy prop_subInverse
    describe "*" $ do
        prop "x * 1 == 1 * x == x" $ lift1 proxy prop_mulUnit
        prop "(x * y) * z == x * (y * z)" $ lift3 proxy prop_mulAssociative
    describe "+, *" $ do
        prop "x * (y + z) == x * y + x * z" $ lift3 proxy prop_leftDistributive
        prop "(x + y) * z == x * z + y * z" $ lift3 proxy prop_rightDistributive
    describe "^" $
        prop "x ^ 0 == 1" $ lift1 proxy prop_zeroPow

additiveAbelianGroupSpec :: (Arbitrary a, Num a, Show a, Eq a) => Proxy a -> Spec
additiveAbelianGroupSpec proxy = do
    describe "+" $ do
        prop "x + 0 == 0 + x == x" $ lift1 proxy prop_addUnit
        prop "(x + y) + z == x + (y + z)" $ lift3 proxy prop_addAssociative
        prop "x + y == y + x" $ lift2 proxy prop_addCommutative

prop_addUnit :: (Num a, Eq a) => a -> Bool
prop_addUnit x = x + 0 == x && 0 + x == x

prop_addAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_addAssociative x y z = (x + y) + z == x + (y + z)

prop_addCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_addCommutative x y = x + y == y + x

prop_subInverse :: (Num a, Eq a) => a -> Bool
prop_subInverse x = x - x == 0

prop_mulUnit :: (Num a, Eq a) => a -> Bool
prop_mulUnit x = x * 1 == x && 1 * x == x

prop_mulAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_mulAssociative x y z = (x * y) * z == x * (y * z)

prop_mulCommutative :: (Num a, Eq a) => a -> a -> Bool
prop_mulCommutative x y = x * y == y * x

prop_leftDistributive :: (Num a, Eq a) => a -> a -> a -> Bool
prop_leftDistributive x y z = x * (y + z)  == x * y + x * z

prop_rightDistributive :: (Num a, Eq a) => a -> a -> a -> Bool
prop_rightDistributive x y z = (x + y) * z  == x * z + y * z

prop_addPow :: (Num a, Eq a) => NonZero a -> NonNegative Int -> NonNegative Int -> Bool
prop_addPow
    (getNonZero -> x)
    (getNonNegative -> n)
    (getNonNegative -> m)
    = x ^ (n + m) == x ^ n * x ^ m

prop_mulPow :: (Num a, Eq a) => NonZero a -> NonNegative Int -> NonNegative Int -> Bool
prop_mulPow
    (getNonZero -> x)
    (getNonNegative -> n)
    (getNonNegative -> m)
    = x ^ (n * m) == (x ^ n) ^ m

prop_powMul :: (Num a, Eq a) => NonZero a -> NonZero a -> NonNegative Int -> Bool
prop_powMul
    (getNonZero -> x)
    (getNonZero -> y)
    (getNonNegative -> n)
    = x ^ n * y ^ n == (x * y) ^ n

prop_zeroPow :: (Num a, Eq a) => a -> Bool
prop_zeroPow x = x ^ 0 == 1
