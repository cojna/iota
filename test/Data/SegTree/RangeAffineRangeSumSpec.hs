{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SegTree.RangeAffineRangeSumSpec where

import Data.Monoid.Affine
import Data.Monoid.AffineSpec ()
import Data.Monoid.RangedSum
import Data.Monoid.RangedSumSpec ()
import Data.SegTree.RangeAffineRangeSum ()
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Affine Int) (RangedSum Int)" $ do
    monoidActionSpec (Proxy @(Affine Int)) (Proxy @(RangedSum Int))
