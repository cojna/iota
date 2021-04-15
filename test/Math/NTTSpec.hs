{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.NTTSpec (main, spec) where

import qualified Data.Vector.Unboxed as U

import Data.GaloisField (GF)
import Data.GaloisFieldSpec ()
import Math.NTT
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ntt/int" $ do
    it "ntt [1,1,1,1] = [4,0,0,0] (mod 998244353)" $ do
      ntt @998244353 [1, 1, 1, 1] `shouldBe` [4, 0, 0, 0]
    it "ntt [123,0,0,0] = [123,123,123,123] (mod 469762049)" $ do
      ntt @469762049 [123, 0, 0, 0] `shouldBe` [123, 123, 123, 123]
    prop "intt . ntt = id (mod 998244353)" prop_nttintt
    prop "ntt . intt = id (mod 998244353)" prop_inttntt
  describe "convolute" $ do
    it "convolute 998244353 3 [1,1,1,0] [1,1,1,0] = [1,2,3,2,1,0,0,0]" $ do
      convolute @998244353 [1, 1, 1, 0] [1, 1, 1, 0]
        `shouldBe` [1, 2, 3, 2, 1, 0, 0]
    it "convolute 998244353 3 [1,1,1] [1,1,1,0] = [1,2,3,2,1,0,0]" $ do
      convolute @998244353 [1, 1, 1] [1, 1, 1, 0]
        `shouldBe` [1, 2, 3, 2, 1, 0]
  describe "growToPowerOfTwo" $ do
    it "growToPowerOfTwo [1,2,3] = [1,2,3,0]" $ do
      growToPowerOfTwo @(GF 998244353) [1, 2, 3] `shouldBe` [1, 2, 3, 0]
  describe "primitiveRoot" $ do
    it "primitiveRoot 2 = 1" $ do
      primitiveRoot 2 `shouldBe` 1
    it "primitiveRoot 167772161 = 3" $ do
      primitiveRoot 167772161 `shouldBe` 3
    it "primitiveRoot 469762049 = 3" $ do
      primitiveRoot 469762049 `shouldBe` 3
    it "primitiveRoot 754974721 = 11" $ do
      primitiveRoot 754974721 `shouldBe` 11
    it "primitiveRoot 998244353 = 3" $ do
      primitiveRoot 998244353 `shouldBe` 3

prop_nttintt :: [GF 998244353] -> Bool
prop_nttintt (growToPowerOfTwo . U.fromList -> v) =
  intt (ntt v) == v

prop_inttntt :: [GF 998244353] -> Bool
prop_inttntt (growToPowerOfTwo . U.fromList -> v) =
  ntt (intt v) == v
