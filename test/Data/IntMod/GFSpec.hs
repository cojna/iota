{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.IntMod.GFSpec where

import           Data.IntMod.GF
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

instance Arbitrary IntMod where
    arbitrary = intMod <$> (arbitrary :: Gen Int)

intModValidate :: IntMod -> Bool
intModValidate x = 0 <= unGF x && unGF x < m
  where
    m = modulus (proxy# :: Proxy# IntMod)

spec :: Spec
spec = do
    describe "+" $ do
        prop "closed" prop_addClosed
        prop "x + 0 == 0 + x == x" prop_addUnit
        prop "(x + y) + z == x + (y + z)" prop_addAssociative
        prop "x + y == y + x" prop_addCommutative
    describe "-" $ do
        prop "closed" prop_subClosed
        prop "x + (0 - x) == 0" prop_subInverse
    describe "negate" $
        prop "closed" prop_nagateClosed
    describe "*" $ do
        prop "closed" prop_mulClosed
        prop "x * 1 == 1 * x == x" prop_mulUnit
        prop "(x * y) * z == x * (y * z)" prop_mulAssociative
        prop "x * y == y * x" prop_mulCommutative
    describe "/" $ do
        prop "closed" prop_divClosed
        prop "x * (1 / x) == 1" prop_divClosed
    describe "recip" $
        prop "closed" prop_recipClosed
    describe "+, *" $ do
        prop "x * (y + z) == x * y + x * z" prop_leftDistributive
        prop "(x + y) * z == x * z + y * z" prop_rightDistributive
    describe "^" $ do
        prop "x ^ (n + m) == (x ^ n) * (x ^ m)" prop_addPow
        prop "x ^ (n * m) == (x ^ n) ^ m" prop_mulPow
        prop "x ^ n * y ^ n == (x * y) ^ n" prop_powMul
        prop "x ^ 0 == 1" prop_zeroPow

prop_addClosed :: IntMod -> IntMod -> Bool
prop_addClosed x y = intModValidate $ x + y

prop_addUnit :: IntMod -> Bool
prop_addUnit x = x + 0 == x && 0 + x == x

prop_addAssociative :: IntMod -> IntMod -> IntMod -> Bool
prop_addAssociative x y z = (x + y) + z == x + (y + z)

prop_addCommutative :: IntMod -> IntMod -> Bool
prop_addCommutative x y = x + y == y + x

prop_subClosed :: IntMod -> IntMod -> Bool
prop_subClosed x y = intModValidate $ x - y

prop_subInverse :: IntMod -> Bool
prop_subInverse x = x - x == intMod 0

prop_nagateClosed :: IntMod -> Bool
prop_nagateClosed x = intModValidate $ negate x

prop_mulClosed :: IntMod -> IntMod -> Bool
prop_mulClosed x y = intModValidate $ x * y

prop_mulUnit :: IntMod -> Bool
prop_mulUnit x = x * intMod 1 == x && intMod 1 * x == x

prop_mulAssociative :: IntMod -> IntMod -> IntMod -> Bool
prop_mulAssociative x y z = (x * y) * z == x * (y * z)

prop_mulCommutative :: IntMod -> IntMod -> Bool
prop_mulCommutative x y = x * y == y * x

prop_divClosed :: IntMod -> IntMod -> Bool
prop_divClosed x y = intModValidate $ x / y

prop_divInverse :: IntMod -> Bool
prop_divInverse x = x * recip x == intMod 1

prop_recipClosed :: IntMod -> Bool
prop_recipClosed x = intModValidate $ recip x

prop_leftDistributive :: IntMod -> IntMod -> IntMod -> Bool
prop_leftDistributive x y z = x * (y + z)  == x * y + x * z

prop_rightDistributive :: IntMod -> IntMod -> IntMod -> Bool
prop_rightDistributive x y z = (x + y) * z  == x * z + y * z

prop_addPow :: NonZero IntMod -> NonNegative Int -> NonNegative Int -> Bool
prop_addPow
    (getNonZero -> x)
    (getNonNegative -> n)
    (getNonNegative -> m)
    = x ^ (n + m) == x ^ n * x ^ m

prop_mulPow :: NonZero IntMod -> NonNegative Int -> NonNegative Int -> Bool
prop_mulPow
    (getNonZero -> x)
    (getNonNegative -> n)
    (getNonNegative -> m)
    = x ^ (n * m) == (x ^ n) ^ m

prop_powMul :: NonZero IntMod -> NonZero IntMod -> NonNegative Int -> Bool
prop_powMul
    (getNonZero -> x)
    (getNonZero -> y)
    (getNonNegative -> n)
    = x ^ n * y ^ n == (x * y) ^ n

prop_zeroPow :: IntMod -> Bool
prop_zeroPow x = x ^ 0 == intMod 1

