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

prop_addClosed :: Int -> Int -> Bool
prop_addClosed x y = intModValidate $ intMod x +% intMod y

prop_addUnit :: Int -> Bool
prop_addUnit x = mx +% 0 == mx && 0 +% mx == mx
  where
    mx = intMod x

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = (mx +% my) +% mz == mx +% (my +% mz)
  where
    mx = intMod x
    my = intMod y
    mz = intMod z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = mx +% my == my +% mx
  where
    mx = intMod x
    my = intMod y

prop_subClosed :: Int -> Int -> Bool
prop_subClosed x y = intModValidate $ intMod x -% intMod y

prop_subInverse :: Int -> Bool
prop_subInverse x = mx +% (0 -% mx) == 0
  where
    mx = intMod x

prop_mulClosed :: Int -> Int -> Bool
prop_mulClosed x y = intModValidate $ intMod x *% intMod y

prop_mulUnit :: Int -> Bool
prop_mulUnit x = mx *% 1 == mx && 1 *% mx == mx
  where
    mx = intMod x

prop_mulAssociative :: Int -> Int -> Int -> Bool
prop_mulAssociative x y z = (mx *% my) *% mz == mx *% (my *% mz)
  where
    mx = intMod x
    my = intMod y
    mz = intMod z

prop_mulCommutative :: Int -> Int -> Bool
prop_mulCommutative x y = mx *% my == my *% mx
  where
    mx = intMod x
    my = intMod y

prop_divClosed :: Int -> Int -> Bool
prop_divClosed x y = intModValidate $ intMod x /% intMod y

prop_divInverse :: Int -> Bool
prop_divInverse x = mx *% (1 /% mx) == 1
  where
    mx = intMod x

prop_leftDistributive :: Int -> Int -> Int -> Bool
prop_leftDistributive x y z = mx *% (my +% mz)  == mx *% my +% mx *% mz
  where
    mx = intMod x
    my = intMod y
    mz = intMod z

prop_rightDistributive :: Int -> Int -> Int -> Bool
prop_rightDistributive x y z = (mx +% my) *% mz  == mx *% mz +% my *% mz
  where
    mx = intMod x
    my = intMod y
    mz = intMod z

prop_addPow :: NonZero Int -> Int -> Int -> Bool
prop_addPow nx n m = mx ^% (n + m) == mx ^% n *% mx ^% m
  where
    x = getNonZero nx
    mx = intMod x

prop_mulPow :: NonZero Int -> Int -> Int -> Bool
prop_mulPow nx n m = mx ^% (n * m) == (mx ^% n) ^% m
  where
    x = getNonZero nx
    mx = intMod x

prop_powMul :: NonZero Int -> NonZero Int -> Int -> Bool
prop_powMul nx ny n = mx ^% n *% my ^% n == (mx *% my) ^% n
  where
    mx = intMod $ getNonZero nx
    my = intMod $ getNonZero ny

prop_zeroPow :: Int -> Bool
prop_zeroPow x = intMod x ^% 0 == 1

prop_negativePow :: Int -> Int -> Bool
prop_negativePow x n = mx ^% negate n == 1 /% mx ^% n
  where
    mx = intMod x
