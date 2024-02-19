module Data.SegTree.RangeMinRangeMinSpec where

import Data.SegTree.RangeMinRangeMin ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Min Int) (Min Int)" $ do
    asSemigroupEndoSpec (Proxy @(Min Int)) (Proxy @(Min Int))
