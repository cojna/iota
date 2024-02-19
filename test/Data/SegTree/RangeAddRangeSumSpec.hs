module Data.SegTree.RangeAddRangeSumSpec where

import Data.Monoid.RangedSum
import Data.Monoid.RangedSumSpec ()
import Data.SegTree.RangeAddRangeSum ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Sum Int) (RangedSum Int)" $ do
    asSemigroupEndoSpec (Proxy @(Sum Int)) (Proxy @(RangedSum Int))
