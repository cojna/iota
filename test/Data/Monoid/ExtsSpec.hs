{-# LANGUAGE ScopedTypeVariables #-}

module Data.Monoid.ExtsSpec (main, spec) where

import Data.Coerce
import Data.Monoid.Exts
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GCD Word" $
    monoidSpec (Proxy :: Proxy (GCD Word))
  describe "LCM (Small Word)" $
    monoidSpec (Proxy :: Proxy (LCM (Small Word)))
  describe "BitAnd Int" $
    monoidSpec (Proxy :: Proxy (BitAnd Int))
  describe "BitOr Int" $
    monoidSpec (Proxy :: Proxy (BitOr Int))
  describe "BitXor Int" $
    monoidSpec (Proxy :: Proxy (BitXor Int))

instance Arbitrary a => Arbitrary (GCD a) where
  arbitrary = (coerce :: a -> GCD a) <$> arbitrary

instance Arbitrary a => Arbitrary (LCM a) where
  arbitrary = (coerce :: a -> LCM a) <$> arbitrary

instance Arbitrary a => Arbitrary (BitAnd a) where
  arbitrary = (coerce :: a -> BitAnd a) <$> arbitrary

instance Arbitrary a => Arbitrary (BitOr a) where
  arbitrary = (coerce :: a -> BitOr a) <$> arbitrary

instance Arbitrary a => Arbitrary (BitXor a) where
  arbitrary = (coerce :: a -> BitXor a) <$> arbitrary
