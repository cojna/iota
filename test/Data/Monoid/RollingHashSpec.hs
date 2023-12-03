{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.RollingHashSpec (main, spec) where

import Data.Monoid.RollingHash
import Data.Proxy
import Data.String
import GHC.TypeLits
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RollingHash 37"
    $ monoidSpec (Proxy :: Proxy (RollingHashBuilder 37))
  describe "RollingHash 2047"
    $ monoidSpec (Proxy :: Proxy (RollingHashBuilder 2047))

instance (KnownNat b) => Arbitrary (RollingHashBuilder b) where
  arbitrary = fromString <$> arbitrary
