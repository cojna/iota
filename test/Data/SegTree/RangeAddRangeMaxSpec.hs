module Data.SegTree.RangeAddRangeMaxSpec where

import Data.SegTree.RangeAddRangeMax ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Sum Int) (Max Int)" $ do
    monoidActionSpec (Proxy @(Sum Int)) (Proxy @(Max Int))
