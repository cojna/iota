{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.SuffixArraySpec (main, spec) where

import qualified Data.ByteString as B
import Data.ByteString.SuffixArray
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U

import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "suffix array" $ do
    prop "naive" prop_naive
    it "buildSuffixArray \"\" == [0]" $ do
      buildSuffixArray B.empty `shouldBe` SuffixArray (U.singleton 0)
  describe "findSubstringsSA" $ do
    describe "abababa" $ do
      let bs = "abababa"
          sa = buildSuffixArray bs
      it "find ab" $ do
        findSubstringsSA bs sa "ab" `shouldBe` U.fromList [4, 2, 0]
      it "find xxx" $ do
        findSubstringsSA bs sa "xxx" `shouldBe` U.empty
      it "find \"\"" $ do
        findSubstringsSA bs sa "" `shouldBe` getSuffixArray sa
      it "find abababa" $ do
        findSubstringsSA bs sa "abababa" `shouldBe` U.singleton 0

prop_naive :: ByteStringOf "ab" -> Bool
prop_naive (getByteStringOf -> bs) =
  map (fromIntegral . indexSA sa) [0 .. n] == naiveSuffixArray bs
  where
    n = B.length bs
    !sa = buildSuffixArray bs

naiveSuffixArray :: B.ByteString -> [Int]
naiveSuffixArray bs = map ((n -) . B.length) . L.sort $ B.tails bs
  where
    !n = B.length bs
