{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SegTree.RangeMulRangeSumSpec where

import Data.SegTree.RangeMulRangeSum ()
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Dual (Product Int)) (Sum Int)" $ do
    monoidActionSpec (Proxy @(Dual (Product Int))) (Proxy @(Sum Int))
