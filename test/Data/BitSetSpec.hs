{-# LANGUAGE OverloadedLists #-}

module Data.BitSetSpec (main, spec) where

import Data.BitSet
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "emptyBS" $ do
    it "emptyBS == []" $ do
      emptyBS `shouldBe` []
  describe "singletonBS" $ do
    it "singletonBS 0 == [0]" $ do
      singletonBS 0 `shouldBe` [0]
    it "singletonBS 32 == [32]" $ do
      singletonBS 32 `shouldBe` [32]
    it "singletonBS 63 == [63]" $ do
      singletonBS 63 `shouldBe` [63]
  describe "insertBS" $ do
    it "insertBS 0 [] == [0]" $ do
      insertBS 0 [] `shouldBe` [0]
    it "insertBS 63 [0] == [0, 63]" $ do
      insertBS 63 [0] `shouldBe` [0, 63]
    it "insertBS 0 [0] == [0]" $ do
      insertBS 0 [0] `shouldBe` [0]
  describe "deleteBS" $ do
    it "deleteBS 1 [0,1,2] == [0,2]" $ do
      deleteBS 1 [0, 1, 2] `shouldBe` [0, 2]
    it "deleteBS 0 [0..63] == [1..63]" $ do
      deleteBS 0 [0 .. 63] `shouldBe` [1 .. 63]
    it "deleteBS 63 [0..63] == [0..62]" $ do
      deleteBS 63 [0 .. 63] `shouldBe` [0 .. 62]
    it "deleteBS 0 [] == []" $ do
      deleteBS 0 [] `shouldBe` []
    it "deleteBS 1 [0] == [0]" $ do
      deleteBS 1 [0] `shouldBe` [0]
  describe "memberBS" $ do
    it "memberBS 0 [] == False" $ do
      memberBS 0 [] `shouldBe` False
    it "memeberBS 0 [0] == True" $ do
      memberBS 0 [0] `shouldBe` True
    it "memberBS 63 [0..63] == True" $ do
      memberBS 63 [0 .. 63] `shouldBe` True
    it "memberBS 63 [0..62] == False" $ do
      memberBS 63 [0 .. 62] `shouldBe` False
  describe "nullBS" $ do
    it "nullBS [] == True" $ do
      nullBS [] `shouldBe` True
    it "nullBS [0] == False" $ do
      nullBS [0] `shouldBe` False
    it "nullBS [0..63] == False" $ do
      nullBS [0 .. 63] `shouldBe` False
  describe "sizeBS" $ do
    it "sizeBS [] == 0" $ do
      sizeBS [] `shouldBe` 0
    it "sizeBS [0] == 1" $ do
      sizeBS [0] `shouldBe` 1
    it "sizeBS [0..63] == 64" $ do
      sizeBS [0 .. 63] `shouldBe` 64
  describe "isSubsetOf" $ do
    it "isSubsetOf [] [] == True" $ do
      isSubsetOf [] [] `shouldBe` True
    it "isSubsetOf [] [1, 2, 3] == True" $ do
      isSubsetOf [] [1, 2, 3] `shouldBe` True
    it "isSubsetOf [1, 2, 3] [2, 3, 4] == False" $ do
      isSubsetOf [1, 2, 3] [2, 3, 4] `shouldBe` False
    it "isSubsetOf [1, 2, 3] [4, 5, 6] == False" $ do
      isSubsetOf [1, 2, 3] [4, 5, 6] `shouldBe` False
    it "isSubsetOf [] [0..63] == True" $ do
      isSubsetOf [] [0 .. 63] `shouldBe` True
    it "isSubsetOf [0..63] [0..63] == True" $ do
      isSubsetOf [0 .. 63] [0 .. 63] `shouldBe` True
  describe "unionBS" $ do
    it "unionBS [] [] == []" $ do
      unionBS [] [] `shouldBe` []
    it "unionBS [1,2,3] [4,5,6] == [1..6]" $ do
      unionBS [1, 2, 3] [4, 5, 6] `shouldBe` [1 .. 6]
    it "unionBS [1,2,3] [1,2,3] == [1,2,3]" $ do
      unionBS [1, 2, 3] [1, 2, 3] `shouldBe` [1, 2, 3]
    it "unionBS [1,2,3] [2,3,4] == [1..4]" $ do
      unionBS [1, 2, 3] [2, 3, 4] `shouldBe` [1 .. 4]
    it "unionBS [] [0..63] == [0..63]" $ do
      unionBS [] [0 .. 63] `shouldBe` [0 .. 63]
    it "unionBS [1,2,3] [0..63] == [0..63]" $ do
      unionBS [1, 2, 3] [0 .. 63] `shouldBe` [0 .. 63]
    it "unionBS [0..63] [0..63] == [0..63]" $ do
      unionBS [0 .. 63] [0 .. 63] `shouldBe` [0 .. 63]
  describe "differenceBS" $ do
    it "differenceBS [] [] == []" $ do
      differenceBS [] [] `shouldBe` []
    it "differenceBS [1,2,3] [4,5,6] == [1,2,3]" $ do
      differenceBS [1, 2, 3] [4, 5, 6] `shouldBe` [1, 2, 3]
    it "differenceBS [1,2,3] [1,2,3] == []" $ do
      differenceBS [1, 2, 3] [1, 2, 3] `shouldBe` []
    it "differenceBS [1,2,3] [2,3,4] == [1]" $ do
      differenceBS [1, 2, 3] [2, 3, 4] `shouldBe` [1]
    it "differenceBS [] [0..63] == []" $ do
      differenceBS [] [0 .. 63] `shouldBe` []
    it "differenceBS [1,2,3] [0..63] == []" $ do
      differenceBS [1, 2, 3] [0 .. 63] `shouldBe` []
    it "differenceBS [0..63] [0..9] == [10..63]" $ do
      differenceBS [0 .. 63] [0 .. 9] `shouldBe` [10 .. 63]
    it "differenceBS [0..63] [0..63] == []" $ do
      differenceBS [0 .. 63] [0 .. 63] `shouldBe` []
  describe "intersectionBS" $ do
    it "intersectionBS [] [] == []" $ do
      intersectionBS [] [] `shouldBe` []
    it "intersectionBS [1,2,3] [4,5,6] == []" $ do
      intersectionBS [1, 2, 3] [4, 5, 6] `shouldBe` []
    it "intersectionBS [1,2,3] [1,2,3] == [1,2,3]" $ do
      intersectionBS [1, 2, 3] [1, 2, 3] `shouldBe` [1, 2, 3]
    it "intersectionBS [1,2,3] [2,3,4] == [2,3]" $ do
      intersectionBS [1, 2, 3] [2, 3, 4] `shouldBe` [2, 3]
    it "intersectionBS [] [0..63] == []" $ do
      intersectionBS [] [0 .. 63] `shouldBe` []
    it "intersectionBS [1,2,3] [0..63] == [1,2,3]" $ do
      intersectionBS [1, 2, 3] [0 .. 63] `shouldBe` [1, 2, 3]
    it "intersectionBS [0..63] [0..63] == [0..63]" $ do
      intersectionBS [0 .. 63] [0 .. 63] `shouldBe` [0 .. 63]
  describe "findMinBS" $ do
    it "findMinBS [] == 64" $ do
      findMinBS [] `shouldBe` 64
    it "findMinBS [0..63] == 0" $ do
      findMinBS [0 .. 63] `shouldBe` 0
    it "findMinBS [1,2,3] == 1" $ do
      findMinBS [1, 2, 3] `shouldBe` 1
    it "findMinBS [63] == 63" $ do
      findMinBS [63] `shouldBe` 63
  describe "findMaxBS" $ do
    it "findMaxBS [] == -1" $ do
      findMaxBS [] `shouldBe` (-1)
    it "findMaxBS [0..63] == 63" $ do
      findMaxBS [0 .. 63] `shouldBe` 63
    it "findMaxBS [1,2,3] == 3" $ do
      findMaxBS [1, 2, 3] `shouldBe` 3
    it "findMaxBS [63] == 63" $ do
      findMaxBS [63] `shouldBe` 63
  describe "deleteMinBS" $ do
    it "deleteMinBS [] == []" $ do
      deleteMinBS [] `shouldBe` []
    it "deleteMinBS [0] == []" $ do
      deleteMinBS [0] `shouldBe` []
    it "deleteMinBS [1,2,3] == [2,3]" $ do
      deleteMinBS [1, 2, 3] `shouldBe` [2, 3]
    it "deleteMinBS [0..63] == [1..63]" $ do
      deleteMinBS [0 .. 63] `shouldBe` [1 .. 63]
    it "deleteMinBS [63] == []" $ do
      deleteMinBS [63] `shouldBe` []
  describe "deleteMaxBS" $ do
    it "deleteMaxBS [] == []" $ do
      deleteMaxBS [] `shouldBe` []
    it "deleteMaxBS [0] == []" $ do
      deleteMaxBS [0] `shouldBe` []
    it "deleteMaxBS [1,2,3] == [1,2]" $ do
      deleteMaxBS [1, 2, 3] `shouldBe` [1, 2]
    it "deleteMaxBS [0..63] == [0..62]" $ do
      deleteMaxBS [0 .. 63] `shouldBe` [0 .. 62]
    it "deleteMaxBS [63] == []" $ do
      deleteMaxBS [63] `shouldBe` []
