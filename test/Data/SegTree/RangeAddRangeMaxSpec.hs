module Data.SegTree.RangeAddRangeMaxSpec where

import Data.SegTree.RangeAddRangeMax ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Sum Int) (Max Int)" $ do
    asSemigroupEndoSpec (Proxy @(Sum Int)) (Proxy @(Max Int))
