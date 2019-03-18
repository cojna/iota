{-# LANGUAGE ViewPatterns #-}

module Data.IntMod.OperatorSpec where

import           Data.Int
import           Data.IntMod.Operator
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec = do
    describe "+%" $ do
        prop "closed" prop_addClosed
        prop "x + 0 == 0 + x == x" prop_addUnit
        prop "(x + y) + z == x + (y + z)" prop_addAssociative
        prop "x + y == y + x" prop_addCommutative
    describe "-%" $ do
        prop "closed" prop_subClosed
        prop "x + (0 - x) == 0" prop_subInverse
    describe "*%" $ do
        prop "closed" prop_mulClosed
        prop "x * 1 == 1 * x == x" prop_mulUnit
        prop "(x * y) * z == x * (y * z)" prop_mulAssociative
        prop "x * y == y * x" prop_mulCommutative
    describe "/%" $ do
        prop "closed" prop_divClosed
        prop "x * (1 / x) == 1" prop_divClosed
    describe "+%, *%" $ do
        prop "x * (y + z) == x * y + x * z" prop_leftDistributive
        prop "(x + y) * z == x * z + y * z" prop_rightDistributive
    describe "^%" $ do
        prop "x ^ (n + m) == (x ^ n) * (x ^ m)" prop_addPow
        prop "x ^ (n * m) == (x ^ n) ^ m" prop_mulPow
        prop "x ^ n * y ^ n == (x * y) ^ n" prop_powMul
        prop "x ^ 0 == 1" prop_zeroPow
        prop "x ^ (-n) == 1 / (x ^ n)" prop_negativePow

prop_addNaive :: Int -> Int -> Bool
prop_addNaive (intMod -> x) (intMod -> y) = x +% y == mod (x + y) modulus

prop_addClosed :: Int -> Int -> Bool
prop_addClosed (intMod -> x) (intMod -> y) = intModValidate $ x +% y

prop_addUnit :: Int -> Bool
prop_addUnit (intMod -> x) = x +% 0 == x && 0 +% x == x

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative
    (intMod -> x)
    (intMod -> y)
    (intMod -> z)
    = (x +% y) +% z == x +% (y +% z)

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative (intMod -> x) (intMod -> y) = x +% y == y +% x

prop_subNaive :: Int -> Int -> Bool
prop_subNaive (intMod -> x) (intMod -> y) = x -% y == mod (x - y) modulus

prop_subClosed :: Int -> Int -> Bool
prop_subClosed (intMod -> x) (intMod -> y) = intModValidate $ x -% y

prop_subInverse :: Int -> Bool
prop_subInverse (intMod -> x) = x +% (0 -% x) == 0

prop_mulNaive :: Int -> Int -> Bool
prop_mulNaive (intMod -> x) (intMod -> y) = x *% y == mod (x * y) modulus

prop_mulClosed :: Int -> Int -> Bool
prop_mulClosed (intMod -> x) (intMod -> y) = intModValidate $ x *% y

prop_mulUnit :: Int -> Bool
prop_mulUnit (intMod -> x) = x *% 1 == x && 1 *% x == x

prop_mulAssociative :: Int -> Int -> Int -> Bool
prop_mulAssociative
    (intMod -> x)
    (intMod -> y)
    (intMod -> z) = (x *% y) *% z == x *% (y *% z)

prop_mulCommutative :: Int -> Int -> Bool
prop_mulCommutative (intMod -> x) (intMod -> y) = x *% y == y *% x

prop_divClosed :: Int -> Int -> Bool
prop_divClosed (intMod -> x) (intMod -> y) = intModValidate $ x /% y

prop_divInverse :: Int -> Bool
prop_divInverse (intMod -> x) = x *% (1 /% x) == 1

prop_leftDistributive :: Int -> Int -> Int -> Bool
prop_leftDistributive
    (intMod -> x)
    (intMod -> y)
    (intMod -> z)
    = x *% (y +% z)  == x *% y +% x *% z

prop_rightDistributive :: Int -> Int -> Int -> Bool
prop_rightDistributive
    (intMod -> x)
    (intMod -> y)
    (intMod -> z)
    = (x +% y) *% z  == x *% z +% y *% z

prop_addPow :: NonZero Int -> Int -> Int -> Bool
prop_addPow (intMod.getNonZero -> x) n m = x ^% (n + m) == x ^% n *% x ^% m

prop_mulPow :: NonZero Int -> Int -> Int -> Bool
prop_mulPow (intMod.getNonZero -> x) n m = x ^% (n * m) == (x ^% n) ^% m

prop_powMul :: NonZero Int -> NonZero Int -> Int -> Bool
prop_powMul
    (intMod.getNonZero -> x)
    (intMod.getNonZero -> y)
    n
    = x ^% n *% y ^% n == (x *% y) ^% n

prop_zeroPow :: Int -> Bool
prop_zeroPow (intMod -> x) = x ^% 0 == 1

prop_negativePow :: Int -> Int -> Bool
prop_negativePow (intMod -> x) n = x ^% negate n == 1 /% x ^% n
