{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.TinySpec where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Tiny

import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unpackTBS . packTBS = id" $ do
    let f :: String -> String
        f = unpackTBS . packTBS
    it "empty" $ do
      f "" `shouldBe` ""
    it "a" $ do
      f "a" `shouldBe` "a"
    it "abcd" $ do
      f "abcd" `shouldBe` "abcd"
    it "abcdefgh" $ do
      f "abcdefgh" `shouldBe` "abcdefgh"
    it "abcdefghi" $ do
      f "abcdefghi" `shouldBe` "abcdefghi"
    it "abcdefghijklmnop" $ do
      f "abcdefghijklmnop" `shouldBe` "abcdefghijklmnop"
  describe "unpackTBS . toTiny . C.pack = id" $ do
    let f :: String -> String
        f = unpackTBS . toTiny . C.pack
    it "empty" $ do
      f "" `shouldBe` ""
    it "a" $ do
      f "a" `shouldBe` "a"
    it "abcd" $ do
      f "abcd" `shouldBe` "abcd"
    it "abcdefgh" $ do
      f "abcdefgh" `shouldBe` "abcdefgh"
    it "abcdefghi" $ do
      f "abcdefghi" `shouldBe` "abcdefghi"
    it "abcdefghijklmnop" $ do
      f "abcdefghijklmnop" `shouldBe` "abcdefghijklmnop"
  describe "lengthTBS" $ do
    it "empty" $ do
      lengthTBS "" `shouldBe` 0
    it "a" $ do
      lengthTBS "a" `shouldBe` 1
    it "abcd" $ do
      lengthTBS "abcd" `shouldBe` 4
    it "abcdefgh" $ do
      lengthTBS "abcdefgh" `shouldBe` 8
    it "abcdefghi" $ do
      lengthTBS "abcdefghi" `shouldBe` 9
    it "abcdefghijklmnop" $ do
      lengthTBS "abcdefghijklmnop" `shouldBe` 16
