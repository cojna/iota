module Data.SegTree.RangeMulRangeSumSpec where

import Data.SegTree.RangeMulRangeSum ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Dual (Product Int)) (Sum Int)" $ do
    asSemigroupEndoSpec (Proxy @(Dual (Product Int))) (Proxy @(Sum Int))
