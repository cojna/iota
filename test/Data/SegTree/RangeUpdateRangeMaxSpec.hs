module Data.SegTree.RangeUpdateRangeMaxSpec where

import Data.Monoid.LastMax
import Data.Monoid.LastMaxSpec ()
import Data.SegTree.RangeUpdateRangeMax ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AsSemigroupEndo (Dual (LastMax a)) (Max a)" $ do
    asSemigroupEndoSpec (Proxy @(Dual (LastMax Int))) (Proxy @(Max Int))
