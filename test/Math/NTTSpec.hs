{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Math.NTTSpec (main, spec) where

import qualified Data.Vector.Unboxed as U

import Math.NTT
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ntt/int" $ do
    it "ntt 998244353 3 [1,1,1,1] = [4,0,0,0]" $ do
      ntt 998244353 3 [1, 1, 1, 1] `shouldBe` [4, 0, 0, 0]
    it "ntt 469762049 3 [123,0,0,0] = [123,123,123,123]" $ do
      ntt 469762049 3 [123, 0, 0, 0] `shouldBe` [123, 123, 123, 123]
    prop "intt . ntt = id (p = 998244353)" prop_nttintt
    prop "ntt . intt = id (p = 998244353)" prop_inttntt
  describe "convolute" $ do
    it "convolute 998244353 3 [1,1,1,0] [1,1,1,0] = [1,2,3,2,1,0,0,0]" $ do
      convolute 998244353 3 [1, 1, 1, 0] [1, 1, 1, 0]
        `shouldBe` [1, 2, 3, 2, 1, 0, 0]
    it "convolute 998244353 3 [1,1,1] [1,1,1,0] = [1,2,3,2,1,0,0]" $ do
      convolute 998244353 3 [1, 1, 1] [1, 1, 1, 0]
        `shouldBe` [1, 2, 3, 2, 1, 0]
  describe "growToPowerOfTwo" $ do
    it "growToPowerOfTwo [1,2,3] = [1,2,3,0]" $ do
      growToPowerOfTwo [1, 2, 3] `shouldBe` [1, 2, 3, 0]

prop_nttintt :: [NonNegative Int] -> Bool
prop_nttintt (growToPowerOfTwo . U.fromList . map getNonNegative -> v) =
  intt 998244353 3 (ntt 998244353 3 v) == v

prop_inttntt :: [NonNegative Int] -> Bool
prop_inttntt (growToPowerOfTwo . U.fromList . map getNonNegative -> v) =
  ntt 998244353 3 (intt 998244353 3 v) == v
