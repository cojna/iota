module Data.Vector2Spec where

import           Data.Proxy
import           Data.Vector2
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.Prop.Num
import           Test.Prop.VectorSpace

spec :: Spec
spec = do
    numSpec (Proxy :: Proxy (Vec2 Int))
    scalarSpec ((*:) :: Int -> Vec2 Int -> Vec2 Int)

instance Arbitrary a => Arbitrary (Vec2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

