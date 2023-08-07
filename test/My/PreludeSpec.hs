{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module My.PreludeSpec (main, spec) where

import Data.ByteString ()
import qualified Data.ByteString.Builder as B
import qualified Data.Vector as V
import My.Prelude
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
    it "matrixB 2 3 [1..6] == \"1 2 3\\n4 5 6\\n\"" $
      B.toLazyByteString (matrixB @V.Vector 2 3 B.intDec [1, 2, 3, 4, 5, 6])
        `shouldBe` "1 2 3\n4 5 6\n"
  describe "gridB" $ do
    it "matrixB 2 3 [1..6] == \"123\\n456\\n\"" $
      B.toLazyByteString (gridB @V.Vector 2 3 B.intDec [1, 2, 3, 4, 5, 6])
        `shouldBe` "123\n456\n"
