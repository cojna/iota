module Data.SegTree.RangeAffineRangeSumSpec where

import Data.Monoid.Affine
import Data.Monoid.AffineSpec ()
import Data.Monoid.RangedSum
import Data.Monoid.RangedSumSpec ()
import Data.SegTree.RangeAffineRangeSum ()
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Affine Int) (RangedSum Int)" $ do
    asSemigroupEndoSpec (Proxy @(Affine Int)) (Proxy @(RangedSum Int))
