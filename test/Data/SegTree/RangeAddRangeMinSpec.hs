{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SegTree.RangeAddRangeMinSpec where

import Data.Semigroup
import Data.SegTree.RangeAddRangeMin ()
import Test.Prelude
import Test.Prop.MonoidAction (monoidActionSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction (Sum Int) (Min Int)" $ do
    monoidActionSpec (Proxy @(Sum Int)) (Proxy @(Min Int))
