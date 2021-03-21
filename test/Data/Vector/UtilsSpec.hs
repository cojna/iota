{-# LANGUAGE OverloadedLists #-}

module Data.Vector.UtilsSpec (main, spec) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Vector.Utils
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "chunks" $ do
    it "chunks 3 [1..6] == [[1,2,3],[4,5,6]]" $ do
      chunks 3 [1 .. 6]
        `shouldBe` ([[1, 2, 3], [4, 5, 6]] :: V.Vector (U.Vector Int))
    it "chunks 3 [1..5] == [[1,2,3],[4,5]]" $ do
      chunks 3 [1 .. 5]
        `shouldBe` ([[1, 2, 3], [4, 5]] :: V.Vector (U.Vector Int))
    it "chunks 3 [] == []" $ do
      chunks 3 []
        `shouldBe` ([] :: V.Vector (U.Vector Int))
  describe "group" $ do
    it "group [1,2,2,3,3,3] == [[1], [2,2], [3,3,3]]" $ do
      group [1, 2, 2, 3, 3, 3]
        `shouldBe` ([[1], [2, 2], [3, 3, 3]] :: V.Vector (U.Vector Int))
    it "group [] == []" $ do
      group []
        `shouldBe` ([] :: V.Vector (U.Vector Int))
  describe "tuples2" $ do
    it "tuples2 [1,2,3,4] == [(1,2), (3,4)]" $ do
      tuples2 [1, 2, 3, 4]
        `shouldBe` ([(1, 2), (3, 4)] :: U.Vector (Int, Int))
    it "tuples2 [1,2,3,4,5] == [(1,2), (3,4)]" $ do
      tuples2 [1, 2, 3, 4, 5]
        `shouldBe` ([(1, 2), (3, 4)] :: U.Vector (Int, Int))
    it "tuples2 [] == []" $ do
      tuples2 []
        `shouldBe` ([] :: U.Vector (Int, Int))
  describe "tuples2N" $ do
    it "tuples2N 2 [1,2,3,4] == [(1,2), (3,4)]" $ do
      tuples2N 2 [1, 2, 3, 4]
        `shouldBe` ([(1, 2), (3, 4)] :: U.Vector (Int, Int))
    it "tuples2N 1 [1,2,3,4] == [(1,2)]" $ do
      tuples2N 1 [1, 2, 3, 4]
        `shouldBe` ([(1, 2)] :: U.Vector (Int, Int))
    it "tuples2N 2 [1,2,3,4,5] == [(1,2), (3,4)]" $ do
      tuples2N 2 [1, 2, 3, 4, 5]
        `shouldBe` ([(1, 2), (3, 4)] :: U.Vector (Int, Int))
    it "tuples2N 0 [] == []" $ do
      tuples2N 0 []
        `shouldBe` ([] :: U.Vector (Int, Int))
  describe "tuples3" $ do
    it "tuples3 [1..6] == [(1,2,3), (4,5,6)]" $ do
      tuples3 [1 .. 6]
        `shouldBe` ([(1, 2, 3), (4, 5, 6)] :: U.Vector (Int, Int, Int))
    it "tuples3 [1..7] == [(1,2,3), (4,5,6)]" $ do
      tuples3 [1 .. 7]
        `shouldBe` ([(1, 2, 3), (4, 5, 6)] :: U.Vector (Int, Int, Int))
    it "tuples3 [] == []" $ do
      tuples3 []
        `shouldBe` ([] :: U.Vector (Int, Int, Int))
