module Math.Linear.Vector2Spec (main, spec) where

import           Data.Proxy
import           Math.Linear.Vector2
import           Test.Prelude
import           Test.Prop.Num
import           Test.Prop.VectorSpace

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    numSpec (Proxy :: Proxy (Vec2 Int))
    scalarSpec ((*:) :: Int -> Vec2 Int -> Vec2 Int)

instance Arbitrary a => Arbitrary (Vec2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

