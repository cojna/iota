module Math.Linear.Matrix2x2Spec (main, spec) where

import           Math.Linear.Matrix2x2
import           Data.Proxy
import           Test.Prelude
import           Test.Prop.Num

main :: IO ()
main = hspec spec

spec :: Spec
spec = numSpec (Proxy :: Proxy (Mat2x2 Int))

instance Arbitrary a => Arbitrary (Mat2x2 a) where
    arbitrary = M22
        <$> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
