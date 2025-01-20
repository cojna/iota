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
    it "floorLog2 maxBound = 62" $ do
      floorLog2 maxBound `shouldBe` 62
    prop "2 ^ floorLog2 n <= n < 2 ^ (floorLog2 n + 1)" prop_floorLog2
  describe "ceilingLog2" $ do
    it "ceilingLog2 1 = 0" $ do
      ceilingLog2 1 `shouldBe` 0
    it "ceilingLog2 2 = 1" $ do
      ceilingLog2 2 `shouldBe` 1
    it "ceilingLog2 3 = 2" $ do
      ceilingLog2 3 `shouldBe` 2
    it "ceilingLog2 1023 = 10" $ do
      ceilingLog2 1023 `shouldBe` 10
    it "ceilingLog2 1024 = 10" $ do
      ceilingLog2 1024 `shouldBe` 10
    it "ceilingLog2 1025 = 11" $ do
      ceilingLog2 1025 `shouldBe` 11
    it "ceilingLog2 maxBound = 63" $ do
      ceilingLog2 maxBound `shouldBe` 63
    prop "floorLog2 x <= ceilingLog2 x"
      $ \(getNonNegative -> x) -> floorLog2 x <= ceilingLog2 x
    prop "x <= 2 ^ ceilingLog2 x"
      $ \(getNonNegative -> x) -> x <= 2 ^ ceilingLog2 x
    prop "2 ^ ceilingLog2 x < 2 * x (x > 0)"
      $ \(getPositive -> x) -> 2 ^ ceilingLog2 x < 2 * x
  describe "floorPowerOf2" $ do
    it "floorPowerOf2 1 = 1" $ do
      floorPowerOf2 1 `shouldBe` 1
    it "floorPowerOf2 2 = 1" $ do
      floorPowerOf2 2 `shouldBe` 2
    it "floorPowerOf2 1023 = 512" $ do
      floorPowerOf2 1023 `shouldBe` 512
    it "floorPowerOf2 1024 = 1024" $ do
      floorPowerOf2 1024 `shouldBe` 1024
    it "floorPowerOf2 1025 = 1024" $ do
      floorPowerOf2 1025 `shouldBe` 1024
    it "floorPowerOf2 maxBound = 2 ^ 62" $ do
      floorPowerOf2 maxBound `shouldBe` (2^(62 :: Int))
    prop "popCount (floorPowerOf2 x) == 1"
      $ \(getNonNegative -> x) -> popCount (floorPowerOf2 x) == 1
    prop "floorPowerOf2 x <= x"
      $ \(getNonNegative -> x) -> floorPowerOf2 x <= x
    prop "x < 2 * floorPowerOf2 x (x > 0)"
      $ \(getPositive -> x) -> x < 2 * floorPowerOf2 x
  describe "ceilingPowerOf2" $ do
    it "ceilingPowerOf2 0 = 1" $ do
      ceilingPowerOf2 0 `shouldBe` 1
    it "ceilingPowerOf2 1 = 1" $ do
      ceilingPowerOf2 1 `shouldBe` 1
    it "ceilingPowerOf2 2 = 1" $ do
      ceilingPowerOf2 2 `shouldBe` 2
    it "ceilingPowerOf2 1023 = 1024" $ do
      ceilingPowerOf2 1023 `shouldBe` 1024
    it "ceilingPowerOf2 1024 = 1024" $ do
      ceilingPowerOf2 1024 `shouldBe` 1024
    it "ceilingPowerOf2 1025 = 2048" $ do
      ceilingPowerOf2 1025 `shouldBe` 2048
    prop "popCount (ceilingPowerOf2 x) == 1"
      $ \(getNonNegative -> x) -> popCount (ceilingPowerOf2 x) == 1
    prop "ceilingPowerOf2 x <= x"
      $ \(getNonNegative -> x) -> x <= ceilingPowerOf2 x
    prop "ceilingPowerOf2 x < 2 * x (x > 0)"
      $ \(getPositive -> x) -> ceilingPowerOf2 x < 2 * x
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
