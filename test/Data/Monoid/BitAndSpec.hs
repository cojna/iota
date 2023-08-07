{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.BitAndSpec (main, spec) where

import Data.Coerce
import Data.Monoid.BitAnd
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BitAnd Int" $
    monoidSpec (Proxy :: Proxy (BitAnd Int))

instance (Arbitrary a) => Arbitrary (BitAnd a) where
  arbitrary = (coerce :: a -> BitAnd a) <$> arbitrary
