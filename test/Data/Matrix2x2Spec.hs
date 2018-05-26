module Data.Matrix2x2Spec where

import           Data.Matrix2x2
import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Util.NumProp

spec :: Spec
spec = numSpec (Proxy :: Proxy (Mat2x2 Int))

instance Arbitrary a => Arbitrary (Mat2x2 a) where
    arbitrary = M22
        <$> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
