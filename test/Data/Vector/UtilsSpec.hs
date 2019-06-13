{-# LANGUAGE OverloadedLists #-}

module Data.Vector.UtilsSpec where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Utils
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "chunks" $ do
        it "chunks 3 [1..6] == [[1,2,3],[4,5,6]]" $ do
            chunks 3 [1..6]
                `shouldBe` ([[1,2,3], [4,5,6]] :: V.Vector (U.Vector Int))
        it "chunks 3 [1..5] == [[1,2,3],[4,5]]" $ do
            chunks 3 [1..5]
                `shouldBe` ([[1,2,3], [4,5]] :: V.Vector (U.Vector Int))
        it "chunks 3 [] == []" $ do
            chunks 3 []
                `shouldBe` ([] :: V.Vector (U.Vector Int))
    describe "group" $ do
        it "group [1,2,2,3,3,3] == [[1], [2,2], [3,3,3]]" $ do
            group [1,2,2,3,3,3]
                `shouldBe` ([[1], [2,2], [3,3,3]] :: V.Vector (U.Vector Int))
        it "group [] == []" $ do
            group []
                `shouldBe` ([] :: V.Vector (U.Vector Int))
    describe "pairs" $ do
        it "pairs [1,2,3,4] == [(1,2), (3,4)]" $ do
            pairs [1,2,3,4]
              `shouldBe` ([(1,2), (3,4)] :: U.Vector (Int, Int))
    describe "pairs" $ do
        it "pairs [1,2,3,4,5] == [(1,2), (3,4)]" $ do
            pairs [1,2,3,4,5]
              `shouldBe` ([(1,2), (3,4)] :: U.Vector (Int, Int))
    describe "pairs" $ do
        it "pairs [] == []" $ do
            pairs []
              `shouldBe` ([] :: U.Vector (Int, Int))
    describe "triples" $ do
        it "triples [1..6] == [(1,2,3), (4,5,6)]" $ do
            triples [1..6]
              `shouldBe` ([(1,2,3), (4,5,6)] :: U.Vector (Int, Int, Int))
    describe "triples" $ do
        it "triples [1..7] == [(1,2,3), (4,5,6)]" $ do
            triples [1..7]
              `shouldBe` ([(1,2,3), (4,5,6)] :: U.Vector (Int, Int, Int))
    describe "triples" $ do
        it "triples [] == []" $ do
            triples []
              `shouldBe` ([] :: U.Vector (Int, Int, Int))
