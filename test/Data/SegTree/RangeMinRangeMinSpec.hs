module Data.SegTree.RangeMinRangeMinSpec where

import Data.SegTree.RangeMinRangeMin ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Min Int) (Min Int)" $ do
    monoidActionSpec (Proxy @(Min Int)) (Proxy @(Min Int))
