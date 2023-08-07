module Data.SegTree.RangeMaxRangeMaxSpec where

import Data.SegTree.RangeMaxRangeMax ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Max Int) (Max Int)" $ do
    monoidActionSpec (Proxy @(Max Int)) (Proxy @(Max Int))
