{-# LANGUAGE OverloadedLists #-}

module Data.IntHeapSpec (main, spec) where

import Data.IntHeap
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "emptyIH" $ do
    it "emptyIH == []" $ do
      emptyIH `shouldBe` []
  describe "singletonIH" $ do
    it "singletonIH 0 == [0]" $ do
      singletonIH 0 `shouldBe` [0]
  describe "insertIH" $ do
    it "insertIH 0 [] == [0]" $ do
      insertIH 0 [] `shouldBe` [0]
    it "insertIH 1 [0] == [0, 1]" $ do
      insertIH 1 [0] `shouldBe` [0, 1]
    it "insertIH 0 [0] == [0, 0]" $ do
      insertIH 0 [0] `shouldBe` [0, 0]
  describe "deleteIH" $ do
    it "deleteIH 0 [0] == []" $ do
      deleteIH 0 [0] `shouldBe` []
    it "deleteIH 0 [0, 0] == [0]" $ do
      deleteIH 0 [0, 0] `shouldBe` [0]
    it "deleteIH 1 [0] == [0]" $ do
      deleteIH 1 [0] `shouldBe` [0]
  describe "findMinIH" $ do
    it "findMinIH [0, 0, 1] == 0" $ do
      findMinIH [0, 0, 1] `shouldBe` 0
    it "findMinIH [] throw exception" $ do
      evaluate (findMinIH []) `shouldThrow` anyErrorCall
  describe "findMaxIH" $ do
    it "findMaxIH [0, 0, 1] == 1" $ do
      findMaxIH [0, 0, 1] `shouldBe` 1
    it "findMaxIH [] throw exception" $ do
      evaluate (findMaxIH []) `shouldThrow` anyErrorCall
  describe "deleteMinIH" $ do
    it "deleteMinIH [0, 0, 1] == [0, 1]" $ do
      deleteMinIH [0, 0, 1] `shouldBe` [0, 1]
    it "deleteMinIH [] throw exception" $ do
      evaluate (deleteMinIH []) `shouldThrow` anyErrorCall
  describe "deleteMaxIH" $ do
    it "deleteMaxIH [0, 0, 1] == [0, 0]" $ do
      deleteMaxIH [0, 0, 1] `shouldBe` [0, 0]
    it "deleteMaxIH [] throw exception" $ do
      evaluate (deleteMaxIH []) `shouldThrow` anyErrorCall
  describe "maxViewIH" $ do
    it "maxViewIH [0, 0, 1] == Just [0, 0]" $ do
      maxViewIH [0, 0, 1] `shouldBe` Just (1, [0, 0])
    it "maxViewIH [] == Nothing" $ do
      maxViewIH [] `shouldBe` Nothing
  describe "minViewIH" $ do
    it "minViewIH [0, 0, 1] == Just [0, 1]" $ do
      minViewIH [0, 0, 1] `shouldBe` Just (0, [0, 1])
    it "minViewIH [] == Nothing" $ do
      minViewIH [] `shouldBe` Nothing
