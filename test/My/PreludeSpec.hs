{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module My.PreludeSpec (main, spec) where

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
