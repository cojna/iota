{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SegTree.RangeAddRangeMaxSpec where

import Data.Semigroup
import Data.SegTree.RangeAddRangeMax ()
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Sum Int) (Max Int)" $ do
    monoidActionSpec (Proxy @(Sum Int)) (Proxy @(Max Int))
