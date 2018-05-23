module Data.Vector2Spec where

import           Data.Proxy
import           Data.Vector2
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Util.NumProp

spec :: Spec
spec = numSpec (Proxy :: Proxy (Vec2 Int))

instance Arbitrary a => Arbitrary (Vec2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

