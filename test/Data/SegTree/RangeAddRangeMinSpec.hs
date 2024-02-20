module Data.SegTree.RangeAddRangeMinSpec where

import Data.SegTree.RangeAddRangeMin ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Sum Int) (Min Int)" $ do
    asSemigroupEndoSpec (Proxy @(Sum Int)) (Proxy @(Min Int))
