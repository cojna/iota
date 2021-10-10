{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.RangedSumSpec (main, spec) where

import Data.Monoid.RangedSum
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RangedSum Int" $
    monoidSpec (Proxy :: Proxy (RangedSum Int))

instance Arbitrary a => Arbitrary (RangedSum a) where
  arbitrary = RangedSum <$> arbitrary <*> arbitrary
