{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.GCDSpec (main, spec) where

import Data.Coerce
import Data.Monoid.GCD
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GCD Word" $
    monoidSpec (Proxy :: Proxy (GCD Word))

instance (Arbitrary a) => Arbitrary (GCD a) where
  arbitrary = (coerce :: a -> GCD a) <$> arbitrary
