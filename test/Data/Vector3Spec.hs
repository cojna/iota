module Data.Vector3Spec (main, spec) where

import           Data.Proxy
import           Data.Vector3
import           Test.Prelude
import           Test.Prop.Num
import           Test.Prop.VectorSpace

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    additiveAbelianGroupSpec (Proxy :: Proxy (Vec3 Int))
    scalarSpec ((*:) :: Int -> Vec3 Int -> Vec3 Int)

instance Arbitrary a => Arbitrary (Vec3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

