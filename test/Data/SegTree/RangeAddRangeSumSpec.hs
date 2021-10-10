{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SegTree.RangeAddRangeSumSpec where

import Data.Monoid.RangedSum
import Data.Monoid.RangedSumSpec ()
import Data.SegTree.RangeAddRangeSum ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Sum Int) (RangedSum Int)" $ do
    monoidActionSpec (Proxy @(Sum Int)) (Proxy @(RangedSum Int))
