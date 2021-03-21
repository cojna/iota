{-# LANGUAGE OverloadedLists #-}

module Data.Vector.CompressSpec where

import Data.Vector.Compress
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "compress" $ do
    it "compress [] = []" $ do
      compress [] `shouldBe` []
    it "compress [0] = [0]" $ do
      compress [0] `shouldBe` [0]
    it "compress [1] = [1]" $ do
      compress [1] `shouldBe` [0]
    it "compress [1, 2, 4, 8, 16] = [0, 1, 2, 3, 4]" $ do
      compress [1, 2, 4, 8, 16] `shouldBe` [0, 1, 2, 3, 4]
    it "compress [4, 16, 2, 8, 1] = [2, 4, 1, 3, 0]" $ do
      compress [4, 16, 2, 8, 1] `shouldBe` [2, 4, 1, 3, 0]
