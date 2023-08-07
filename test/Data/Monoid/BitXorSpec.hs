{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.BitXorSpec (main, spec) where

import Data.Coerce
import Data.Monoid.BitXor
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BitXor Int" $
    monoidSpec (Proxy :: Proxy (BitXor Int))

instance (Arbitrary a) => Arbitrary (BitXor a) where
  arbitrary = (coerce :: a -> BitXor a) <$> arbitrary
