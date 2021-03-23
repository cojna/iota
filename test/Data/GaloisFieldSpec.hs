{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GaloisFieldSpec (main, spec) where

import Data.GaloisField
import Test.Prelude
import Test.Prop.Fractional
import Test.Prop.Num

instance KnownNat p => Arbitrary (GF p) where
  arbitrary = mkGF <$> (arbitrary :: Gen Int)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GF 2" $ do
    numSpec (Proxy @(GF 2))
    fractionalSpec (Proxy @(GF 2))
  describe "GF 998244353" $ do
    numSpec (Proxy @(GF 998244353))
    fractionalSpec (Proxy @(GF 998244353))
  describe "GF 1000000007" $ do
    numSpec (Proxy @(GF 1000000007))
    fractionalSpec (Proxy @(GF 1000000007))
