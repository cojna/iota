{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module My.PreludeSpec (main, spec) where

import Control.Monad.State.Strict
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
  describe "takeLine @Maybe" $ do
    it "runStateT takeLine \"abc\" == Just (\"abc\",\"\")" $ do
      runStateT @_ @Maybe takeLine "abc"
        `shouldBe` Just ("abc", "")
    it "runStateT takeLine \"abc\\n\" == Just (\"abc\",\"\")" $ do
      runStateT @_ @Maybe takeLine "abc\n"
        `shouldBe` Just ("abc", "")
    it "runStateT takeLine \"abc\\r\\n\" == Just (\"abc\\r\",\"\")" $ do
      runStateT @_ @Maybe takeLine "abc\r\n"
        `shouldBe` Just ("abc\r", "")
    it "runStateT takeLine \"\" == Just (\"\",\"\")" $ do
      runStateT @_ @Maybe takeLine ""
        `shouldBe` Just ("", "")
    it "runStateT takeLine \"\\n\" == Just (\"\",\"\")" $ do
      runStateT @_ @Maybe takeLine "\n"
        `shouldBe` Just ("", "")
    it "runStateT takeLine \"\\n\\n\" == Just (\"\",\"\\n\")" $ do
      runStateT @_ @Maybe takeLine "\n\n"
        `shouldBe` Just ("", "\n")
  describe "takeLines @Maybe" $ do
    it "runStateT takeLines 1 \"abc\\ndef\\n\" == Just (\"abc\\n\",\"def\\n\")" $ do
      runStateT @_ @Maybe (takeLines 1) "abc\ndef\n"
        `shouldBe` Just ("abc\n", "def\n")
    it "runStateT takeLines 2 \"abc\\ndef\\n\" == Just (\"abc\\ndef\\n\",\"\")" $ do
      runStateT @_ @Maybe (takeLines 2) "abc\ndef\n"
        `shouldBe` Just ("abc\ndef\n", "")
    it "runStateT takeLines 0 \"abc\\ndef\\n\" == Just (\"\",\"abc\\ndef\\n\")" $ do
      runStateT @_ @Maybe (takeLines 0) "abc\ndef\n"
        `shouldBe` Just ("", "abc\ndef\n")
    it "runStateT takeLines 2 \"abc\\ndef\" == Just (\"abc\\ndef\",\"\")" $ do
      runStateT @_ @Maybe (takeLines 2) "abc\ndef"
        `shouldBe` Just ("abc\ndef", "")
    it "runStateT takeLines 999 \"abc\" == Just (\"abc\",\"\")" $ do
      runStateT @_ @Maybe (takeLines 999) "abc"
        `shouldBe` Just ("abc", "")
