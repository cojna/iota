module Data.Matrix3x3Spec where

import           Data.Matrix3x3
import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.Prop.Num

spec :: Spec
spec = numSpec (Proxy :: Proxy (Mat3x3 Int))

instance Arbitrary a => Arbitrary (Mat3x3 a) where
    arbitrary = M33
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
