module Data.Vector3Spec where

import           Data.Proxy
import           Data.Vector3
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Util.NumProp

spec :: Spec
spec = additiveAbelianGroupSpec (Proxy :: Proxy (Vec3 Int))

instance Arbitrary a => Arbitrary (Vec3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

