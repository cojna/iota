{-# LANGUAGE OverloadedLists #-}

module Algorithm.LISSpec where

import Algorithm.LIS
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "longestIncreasingSubsequence" $ do
    it "lis [] = 0" $ do
      longestIncreasingSubsequence []
        `shouldBe` 0
    it "lis [0] = 1" $ do
      longestIncreasingSubsequence [0]
        `shouldBe` 1
    it "lis [0, 1] = 2" $ do
      longestIncreasingSubsequence [0, 1]
        `shouldBe` 2
    it "lis [1, 0] = 1" $ do
      longestIncreasingSubsequence [1, 0]
        `shouldBe` 1
    it "lis [0, 0] = 1" $ do
      longestIncreasingSubsequence [0, 0]
        `shouldBe` 1
    it "lis [1, 0, 2, 0, 3] = 3" $ do
      longestIncreasingSubsequence [1, 0, 2, 0, 3]
        `shouldBe` 3
