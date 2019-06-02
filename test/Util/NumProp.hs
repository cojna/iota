{-# LANGUAGE ViewPatterns #-}

module Util.NumProp where

import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary


numSpec :: (Arbitrary a, Num a, Show a, Eq a) => Proxy a -> Spec
numSpec proxy = do
    describe "+" $ do
        prop "x + 0 == 0 + x == x" $ prop_addUnit proxy
        prop "(x + y) + z == x + (y + z)" $ prop_addAssociative proxy
        prop "x + y == y + x" $ prop_addCommutative proxy
    describe "-" $
        prop "x + (0 - x) == 0" $ prop_subInverse proxy
    describe "*" $ do
        prop "x * 1 == 1 * x == x" $ prop_mulUnit proxy
        prop "(x * y) * z == x * (y * z)" $ prop_mulAssociative proxy
    describe "+, *" $ do
        prop "x * (y + z) == x * y + x * z" $ prop_leftDistributive proxy
        prop "(x + y) * z == x * z + y * z" $ prop_rightDistributive proxy
    describe "^" $
        prop "x ^ 0 == 1" $ prop_zeroPow proxy

additiveAbelianGroupSpec :: (Arbitrary a, Num a, Show a, Eq a) => Proxy a -> Spec
additiveAbelianGroupSpec proxy = do
    describe "+" $ do
        prop "x + 0 == 0 + x == x" $ prop_addUnit proxy
        prop "(x + y) + z == x + (y + z)" $ prop_addAssociative proxy
        prop "x + y == y + x" $ prop_addCommutative proxy

prop_addUnit :: (Num a, Eq a) => Proxy a -> a -> Bool
prop_addUnit _ x = x + 0 == x && 0 + x == x

prop_addAssociative :: (Num a, Eq a) => Proxy a -> a -> a -> a -> Bool
prop_addAssociative _ x y z = (x + y) + z == x + (y + z)

prop_addCommutative :: (Num a, Eq a) => Proxy a -> a -> a -> Bool
prop_addCommutative _ x y = x + y == y + x

prop_subInverse :: (Num a, Eq a) => Proxy a -> a -> Bool
prop_subInverse _ x = x - x == 0

prop_mulUnit :: (Num a, Eq a) => Proxy a -> a -> Bool
prop_mulUnit _ x = x * 1 == x && 1 * x == x

prop_mulAssociative :: (Num a, Eq a) => Proxy a -> a -> a -> a -> Bool
prop_mulAssociative _ x y z = (x * y) * z == x * (y * z)

prop_mulCommutative :: (Num a, Eq a) => Proxy a -> a -> a -> Bool
prop_mulCommutative _ x y = x * y == y * x

prop_leftDistributive :: (Num a, Eq a) => Proxy a -> a -> a -> a -> Bool
prop_leftDistributive _ x y z = x * (y + z)  == x * y + x * z

prop_rightDistributive :: (Num a, Eq a) => Proxy a -> a -> a -> a -> Bool
prop_rightDistributive _ x y z = (x + y) * z  == x * z + y * z

prop_addPow
    :: (Num a, Eq a)
    => Proxy a -> NonZero a -> NonNegative Int -> NonNegative Int -> Bool
prop_addPow _
    (getNonZero -> x)
    (getNonNegative -> n)
    (getNonNegative -> m)
    = x ^ (n + m) == x ^ n * x ^ m

prop_mulPow
    :: (Num a, Eq a)
    => Proxy a -> NonZero a -> NonNegative Int -> NonNegative Int -> Bool
prop_mulPow _
    (getNonZero -> x)
    (getNonNegative -> n)
    (getNonNegative -> m)
    = x ^ (n * m) == (x ^ n) ^ m

prop_powMul
    :: (Num a, Eq a)
    => Proxy a -> NonZero a -> NonZero a -> NonNegative Int -> Bool
prop_powMul _
    (getNonZero -> x)
    (getNonZero -> y)
    (getNonNegative -> n)
    = x ^ n * y ^ n == (x * y) ^ n

prop_zeroPow :: (Num a, Eq a) => Proxy a -> a -> Bool
prop_zeroPow _ x = x ^ 0 == 1
