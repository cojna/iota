{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module My.PreludeSpec (main, spec) where

import Data.Bits
import Data.ByteString ()
import qualified Data.ByteString.Builder as B
import Data.PrimParser
import qualified Data.Vector as V
import My.Prelude
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "floorLog2" $ do
    it "floorLog2 1 = 0" $ do
      floorLog2 1 `shouldBe` 0
    it "floorLog2 2 = 1" $ do
      floorLog2 2 `shouldBe` 1
    it "floorLog2 3 = 1" $ do
      floorLog2 3 `shouldBe` 1
    it "floorLog2 1023 = 9" $ do
      floorLog2 1023 `shouldBe` 9
    it "floorLog2 1024 = 10" $ do
      floorLog2 1024 `shouldBe` 10
    it "floorLog2 1025 = 10" $ do
      floorLog2 1025 `shouldBe` 10
    prop "2 ^ n <= floorLog2 n < 2 ^ (n + 1)" prop_floorLog2
#if __GLASGOW_HASKELL__ > 904
  describe "uvectorLn" $ do
    it "uvectorLn \"abc\\n\" == ['a', 'b', 'c']" $
      unsafeWithByteString "abc\n" (uvectorLn char)
        `shouldBe` ['a', 'b', 'c']
    it "line $ uvectorLn \"abc\\n\" == ['a', 'b', 'c']" $
      unsafeWithByteString "abc\n" (line $ uvectorLn char)
        `shouldBe` ['a', 'b', 'c']
    it "uvectorLn \"abc\\n\\n\" == ['a', 'b', 'c']" $
      unsafeWithByteString "abc\n\n" (uvectorLn char)
        `shouldBe` ['a', 'b', 'c']
    it "line $ uvectorLn \"abc\\n\\n\" == ['a', 'b', 'c']" $
      unsafeWithByteString "abc\n\n" (line $ uvectorLn char)
        `shouldBe` ['a', 'b', 'c']
#endif
  describe "unwordsB" $ do
    it "unwordsB intDec [1,2,3] == \"1 2 3\"" $
      B.toLazyByteString (unwordsB @V.Vector B.intDec [1, 2, 3])
        `shouldBe` "1 2 3"
  describe "unlinesB" $ do
    it "unlinesB intDec [1,2,3] == \"1\\n2\\n3\\n\"" $
      B.toLazyByteString (unlinesB @V.Vector B.intDec [1, 2, 3])
        `shouldBe` "1\n2\n3\n"
    it "unlinesB (pairB intDec intDec) [(1,2),(3,4)] == \"1 2\\n3 4\\n\"" $
      B.toLazyByteString (unlinesB @V.Vector (pairB B.intDec B.intDec) [(1, 2), (3, 4)])
        `shouldBe` "1 2\n3 4\n"
  describe "matrixB" $ do
    it "matrixB (2, 3, [1..6]) == \"1 2 3\\n4 5 6\\n\"" $
      B.toLazyByteString (matrixB @V.Vector B.intDec (2, 3, [1, 2, 3, 4, 5, 6]))
        `shouldBe` "1 2 3\n4 5 6\n"
  describe "gridB" $ do
    it "gridB (2, 3, [1..6]) == \"123\\n456\\n\"" $
      B.toLazyByteString (gridB @V.Vector B.intDec (2, 3, [1, 2, 3, 4, 5, 6]))
        `shouldBe` "123\n456\n"

prop_floorLog2 :: Positive Int -> Bool
prop_floorLog2 (getPositive -> n) =
  shiftL 1 res <= n && n < shiftL 1 (res + 1)
  where
    res = floorLog2 n
