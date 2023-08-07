{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.AffineSpec (main, spec) where

import Data.Monoid.Affine
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Affine Int" $
    monoidSpec (Proxy :: Proxy (Affine Int))

instance (Arbitrary a) => Arbitrary (Affine a) where
  arbitrary = Affine <$> arbitrary <*> arbitrary
