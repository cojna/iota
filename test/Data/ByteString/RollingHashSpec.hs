{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.RollingHashSpec where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.ByteString.RollingHash
import Data.RollingHash
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rollingHash" $ do
    prop "naive" prop_naive
  describe "rollingHashWith" $ do
    prop "naive" prop_naiveWithBase
  describe "isPrimitiveRootRH" $ do
    it "all (not.isPrimitiveRootRH) [2..36]" $ do
      not (any isPrimitiveRootRH [2 .. 36]) `shouldBe` True
    it "isPrimitiveRootRH 37" $ do
      isPrimitiveRootRH 37 `shouldBe` True
    it "isPrimitiveRootRH 2047" $ do
      isPrimitiveRootRH 2047 `shouldBe` True

prop_naive :: PrintableString -> Bool
prop_naive (C.pack . getPrintableString -> bs) =
  getRollingHash (rollingHash bs) == rollingHashNaiveWith 2047 bs

prop_naiveWithBase :: Modulo 0x1fffffffffffffff Int -> PrintableString -> Bool
prop_naiveWithBase
  (getModulo -> base)
  (C.pack . getPrintableString -> bs) = case someNatVal (fromIntegral base) of
    Just (SomeNat proxy) ->
      getRollingHash (rollingHashWith bs `asRollingHashOf` proxy)
        == rollingHashNaiveWith base bs
    Nothing -> False

rollingHashNaiveWith :: Int -> B.ByteString -> Int
rollingHashNaiveWith base = fromInteger . B.foldl' step 0
  where
    m = 0x1fffffffffffffff
    step acc w8 = rem (acc * toInteger base + fromIntegral w8) m
