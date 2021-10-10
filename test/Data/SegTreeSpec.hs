{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SegTreeSpec (main, spec) where

import Data.IntMod
import Data.Monoid hiding (First (..), Last (..))
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction () (Product IntMod)" $ do
    monoidActionSpec (Proxy @()) (Proxy @(Product IntMod))
  describe "MonoidAction (Min Int) (Min Int)" $ do
    monoidActionSpec (Proxy @(Min Int)) (Proxy @(Min Int))
  describe "MonoidAction (Max Int) (Max Int)" $ do
    monoidActionSpec (Proxy @(Max Int)) (Proxy @(Max Int))
