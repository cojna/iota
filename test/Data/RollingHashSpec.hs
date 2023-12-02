{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.RollingHashSpec (main, spec) where

import Data.Coerce
import Data.RollingHash
import Test.Prelude
import Test.Prop.Num

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RollingHash 2047" $ do
    numSpec (Proxy @(RollingHash 2047))
  describe "isPrimitiveRootRH" $ do
    it "all (not.isPrimitiveRootRH) [2..36]" $ do
      not (any isPrimitiveRootRH [2 .. 36]) `shouldBe` True
    it "isPrimitiveRootRH 37" $ do
      isPrimitiveRootRH 37 `shouldBe` True
    it "isPrimitiveRootRH 2047" $ do
      isPrimitiveRootRH 2047 `shouldBe` True

instance Arbitrary (RollingHash b) where
  arbitrary = coerce (arbitrary @(Modulo 0x1fffffffffffffff Int))