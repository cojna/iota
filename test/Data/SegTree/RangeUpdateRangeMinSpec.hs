module Data.SegTree.RangeUpdateRangeMinSpec where

import Data.Monoid.LastMin
import Data.Monoid.LastMinSpec ()
import Data.SegTree.RangeUpdateRangeMin ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Dual (LastMin a)) (Min a)" $ do
    monoidActionSpec (Proxy @(Dual (LastMin Int))) (Proxy @(Min Int))
