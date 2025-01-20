{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.PrimParserSpec where

import Control.Monad
import Data.ByteString ()
import Data.PrimParser
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "int" $ do
    it "-123\\n" $ do
      unsafeWithByteString "-123\n" int `shouldBe` (-123)
    it "-123 " $ do
      unsafeWithByteString "-123 " int `shouldBe` (-123)
    it "123\\n" $ do
      unsafeWithByteString "123\n" int `shouldBe` 123
    it "0\\n" $ do
      unsafeWithByteString "0\n" int `shouldBe` 0
    it "-0\\n" $ do
      unsafeWithByteString "-0\n" int `shouldBe` 0
    it "0\\n" $ do
      unsafeWithByteString "0\n" int `shouldBe` 0
    it "-9223372036854775808\\n" $ do
      unsafeWithByteString "-9223372036854775808\n" int `shouldBe` (-9223372036854775808)
    it "9223372036854775807\\n" $ do
      unsafeWithByteString "9223372036854775807\n" int `shouldBe` 9223372036854775807
  describe "uint" $ do
    it "123\\n" $ do
      unsafeWithByteString "123\n" uint `shouldBe` 123
    it "123 " $ do
      unsafeWithByteString "123 " uint `shouldBe` 123
    it "0\\n" $ do
      unsafeWithByteString "0\n" uint `shouldBe` 0
    it "9223372036854775807\\n" $ do
      unsafeWithByteString "9223372036854775807\n" int `shouldBe` 9223372036854775807
  describe "uint1" $ do
    it "123\\n" $ do
      unsafeWithByteString "123\n" uint1 `shouldBe` 122
    it "123 " $ do
      unsafeWithByteString "123 " uint1 `shouldBe` 122
    it "0\\n" $ do
      unsafeWithByteString "0\n" uint1 `shouldBe` (-1)
    it "1\\n" $ do
      unsafeWithByteString "1\n" uint1 `shouldBe` 0
    it "9223372036854775807\\n" $ do
      unsafeWithByteString "9223372036854775807\n" uint1 `shouldBe` 9223372036854775806
  describe "double" $ do
    it "-123.456789\\n" $ do
      unsafeWithByteString "-123.456789\n" double `shouldBe` (-123.456789)
    it "-123.456789 " $ do
      unsafeWithByteString "-123.456789 " double `shouldBe` (-123.456789)
    it "123.456789\\n" $ do
      unsafeWithByteString "123.456789\n" double `shouldBe` 123.456789
    it "123\\n" $ do
      unsafeWithByteString "123\n" double `shouldBe` 123
    it "123.0\\n" $ do
      unsafeWithByteString "123.0\n" double `shouldBe` 123.0
    it "-123\\n" $ do
      unsafeWithByteString "-123\n" double `shouldBe` (-123)
    it "-123.0\\n" $ do
      unsafeWithByteString "-123.0\n" double `shouldBe` (-123.0)
    it "0\\n" $ do
      unsafeWithByteString "0\n" double `shouldBe` 0
    it "0.0\\n" $ do
      unsafeWithByteString "0.0\n" double `shouldBe` 0.0
  describe "char" $ do
    it "a\\n" $ do
      unsafeWithByteString "a\n" char `shouldBe` 'a'
    it "a " $ do
      unsafeWithByteString "a " char `shouldBe` 'a'
    it "A\\n" $ do
      unsafeWithByteString "A\n" char `shouldBe` 'A'
    it "0\\n" $ do
      unsafeWithByteString "0\n" char `shouldBe` '0'
    it "#\\n" $ do
      unsafeWithByteString "#\n" char `shouldBe` '#'
    it ".\\n" $ do
      unsafeWithByteString ".\n" char `shouldBe` '.'
    it "\\n\\n" $ do
      unsafeWithByteString "\n\n" char `shouldBe` '\n'
    it " \\n" $ do
      unsafeWithByteString " \n" char `shouldBe` ' '
  describe "byteStringLn" $ do
    it "abc\\n" $ do
      unsafeWithByteString "abc\n" byteStringLn `shouldBe` "abc"
    it "abc\\ndef\\n" $ do
      unsafeWithByteString "abc\ndef\n" byteStringLn `shouldBe` "abc"
    it "\\n" $ do
      unsafeWithByteString "\n" byteStringLn `shouldBe` ""
    it " \\n" $ do
      unsafeWithByteString " \n" byteStringLn `shouldBe` " "
  describe "byteStringSp" $ do
    it "abc " $ do
      unsafeWithByteString "abc " byteStringSp `shouldBe` "abc"
    it "abc def\\n" $ do
      unsafeWithByteString "abc def\\n" byteStringSp `shouldBe` "abc"
    it " " $ do
      unsafeWithByteString " " byteStringSp `shouldBe` ""
    it " \\n" $ do
      unsafeWithByteString " \n" byteStringSp `shouldBe` ""
  describe "byteStringN" $ do
    it "abcdef\\n" $ do
      unsafeWithByteString
        "abcdef\n"
        ((,) <$> byteStringN 3 <*> viewPrimParserAsByteString)
        `shouldBe` ("abc", "def\n")
  describe "byteArrayN" $ do
    it "byteArrayN 3 \"abcdef\" == \"abc\"" $
      unsafeWithByteString "abcdef" (byteArrayN 3)
        `shouldBe` [97, 98, 99]
    it "byteArrayN 0 \"abcdef\" == \"\"" $
      unsafeWithByteString "abcdef" (byteArrayN 0)
        `shouldBe` []
    it "byteArrayN 6 \"abcdef\" == \"abcdef\"" $
      unsafeWithByteString "abcdef" (byteArrayN 6)
        `shouldBe` [97, 98, 99, 100, 101, 102]
  describe "byteArrayHW" $ do
    it "byteArrayHW 3 2 \"ab\\ncd\\nef\\n\" == \"abcdef\"" $
      unsafeWithByteString "ab\ncd\nef\\n" (byteArrayHW 3 2)
        `shouldBe` [97, 98, 99, 100, 101, 102]
    it "byteArrayHW 3 2 \"ab\\ncd\\nef\" == \"abcdef\"" $
      unsafeWithByteString "ab\ncd\nef" (byteArrayHW 3 2)
        `shouldBe` [97, 98, 99, 100, 101, 102]
    it "byteArrayHW 2 2 \"ab\\ncd\\nef\\n\" == \"abcdef\"" $
      unsafeWithByteString "ab\ncd\n" (byteArrayHW 2 2)
        `shouldBe` [97, 98, 99, 100]
  describe "tuples" $ do
    it "(int, int, int): 1 2 3\\n" $ do
      unsafeWithByteString "1 2 3\n" ((,,) <$> int <*> int <*> int)
        `shouldBe` (1, 2, 3)
    it "(int, [int]): 3\\n-1 0 1\\n" $ do
      unsafeWithByteString "3\n-1 0 1\n" (do n <- int; replicateM n int)
        `shouldBe` [-1, 0, 1]
    it "(int, [uint1]: 3\\n1 2 3\\n" $ do
      unsafeWithByteString "3\n1 2 3\n" (do n <- int; replicateM n uint1)
        `shouldBe` [0, 1, 2]
    it "(int, [(int, [uint1])]): 3\\n3 1 2 3\\n0\\n2 4 5\\n" $ do
      unsafeWithByteString
        "3\n3 1 2 3\n0\n2 4 5\n"
        (do n <- int; replicateM n (do l <- int; replicateM l uint1))
        `shouldBe` [[0, 1, 2], [], [3, 4]]
    it "(charSp, uint1, uint): a 1 100\\n" $ do
      unsafeWithByteString "a 1 100\n" ((,,) <$> charSp <*> uint1 <*> uint)
        `shouldBe` ('a', 0, 100)
    it "(digitC, digitC): 09\\n" $ do
      unsafeWithByteString "09\n" ((,) <$> digitC <*> digitC)
        `shouldBe` (0, 9)
    it "(lowerC, lowerC): az\\n" $ do
      unsafeWithByteString "az\n" ((,) <$> lowerC <*> lowerC)
        `shouldBe` (0, 25)
    it "(upperC, upperC): AZ\\n" $ do
      unsafeWithByteString "AZ\n" ((,) <$> upperC <*> upperC)
        `shouldBe` (0, 25)
    it "(byteString, byteString): abc def\\n" $ do
      unsafeWithByteString "abc def\n" ((,) <$> byteStringSp <*> byteStringLn)
        `shouldBe` ("abc", "def")
    it "(int, [bytestring]): 3\\nabc\\ndef\\nghi==n" $ do
      unsafeWithByteString "3\nabc\ndef\nghi\n" (do n <- int; replicateM n byteStringLn)
        `shouldBe` ["abc", "def", "ghi"]
    it "(int, [(double, double)]): 2\\n-1 -1.0\\n0 1.25\\n" $ do
      unsafeWithByteString
        "2\n-1 -1.0\n0 1.25\n"
        (do n <- int; replicateM n $ (,) <$> double <*> double)
        `shouldBe` [(-1, -1), (0, 1.25)]
  describe "line" $ do
    it "123\\n456\\n" $ do
      unsafeWithByteString "123\n456\n" ((,) <$> line int <*> int)
        `shouldBe` (123, 456)
    it "abc\\ndef\\n" $ do
      unsafeWithByteString
        "abc\ndef\n"
        ((,) <$> line viewPrimParserAsByteString <*> viewPrimParserAsByteString)
        `shouldBe` ("abc\n", "def\n")
  describe "linesN" $ do
    it "123\\n456\\n789\\n" $ do
      unsafeWithByteString "123\n456\n789\n" ((,) <$> linesN 2 (pure ()) <*> int)
        `shouldBe` ((), 789)
    it "abc\\ndef\\nghi\\n" $ do
      unsafeWithByteString
        "abc\ndef\nghi\n"
        ((,) <$> linesN 2 viewPrimParserAsByteString <*> viewPrimParserAsByteString)
        `shouldBe` ("abc\ndef\n", "ghi\n")
    it "linesN 0" $ do
      unsafeWithByteString
        "linesN 0\n"
        ((,) <$> linesN 0 viewPrimParserAsByteString <*> viewPrimParserAsByteString)
        `shouldBe` ("", "linesN 0\n")
