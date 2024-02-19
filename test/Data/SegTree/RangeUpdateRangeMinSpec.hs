module Data.SegTree.RangeUpdateRangeMinSpec where

import Data.Monoid.LastMin
import Data.Monoid.LastMinSpec ()
import Data.SegTree.RangeUpdateRangeMin ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Dual (LastMin a)) (Min a)" $ do
    asSemigroupEndoSpec (Proxy @(Dual (LastMin Int))) (Proxy @(Min Int))
