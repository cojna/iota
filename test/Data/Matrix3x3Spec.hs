module Data.Matrix3x3Spec (main, spec) where

import           Data.Matrix3x3
import           Data.Proxy
import           Test.Prelude
import           Test.Prop.Num

main :: IO ()
main = hspec spec

spec :: Spec
spec = numSpec (Proxy :: Proxy (Mat3x3 Int))

instance Arbitrary a => Arbitrary (Mat3x3 a) where
    arbitrary = M33
        <$> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
