module Data.SegTree.RangeUpdateRangeMaxSpec where

import Data.Monoid.LastMax
import Data.Monoid.LastMaxSpec ()
import Data.SegTree.RangeUpdateRangeMax ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Dual (LastMax a)) (Max a)" $ do
    monoidActionSpec (Proxy @(Dual (LastMax Int))) (Proxy @(Max Int))
