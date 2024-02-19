module Data.SegTree.RangeMaxRangeMaxSpec where

import Data.SegTree.RangeMaxRangeMax ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Max Int) (Max Int)" $ do
    asSemigroupEndoSpec (Proxy @(Max Int)) (Proxy @(Max Int))
