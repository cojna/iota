{-# LANGUAGE OverloadedLists #-}

module Data.SparseTableSpec (main, spec) where

import Data.SparseTable
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RMQ" $ do
    let rmq = buildRMQ @Int [5, 4, 3, 2, 1]
    describe "buildRMQ [5, 4, 3, 2, 1]" $ do
      it "queryMin 0 5 == 1" $ do
        queryMin rmq 0 5 `shouldBe` 1
      it "queryMin 0 4 == 2" $ do
        queryMin rmq 0 4 `shouldBe` 2
      it "queryMin 0 1 == 5" $ do
        queryMin rmq 0 1 `shouldBe` 5
      it "queryMin 4 5 == 1" $ do
        queryMin rmq 4 5 `shouldBe` 1
