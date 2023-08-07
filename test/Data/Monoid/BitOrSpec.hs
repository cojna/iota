{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.BitOrSpec (main, spec) where

import Data.Coerce
import Data.Monoid.BitOr
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BitOr Int" $
    monoidSpec (Proxy :: Proxy (BitOr Int))

instance (Arbitrary a) => Arbitrary (BitOr a) where
  arbitrary = (coerce :: a -> BitOr a) <$> arbitrary
