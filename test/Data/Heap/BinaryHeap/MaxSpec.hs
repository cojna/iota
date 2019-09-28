{-# LANGUAGE OverloadedLists #-}

module Data.Heap.BinaryHeap.MaxSpec where

import           Control.Monad
import           Data.Function
import           Data.Heap.BinaryHeap.Max
import qualified Data.List                as L
import           Data.Maybe
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed      as U
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

spec :: Spec
spec = do
    describe "siftUp" $ do
        it "siftUp [0, 1, 2] 1 == [1, 0, 2]" $ do
            U.modify (siftUp 1) [0 :: Int, 1, 2]
             `shouldBe` [1 :: Int, 0, 2]
        it "siftUp [0, 1, 2] 2 == [2, 1, 0]" $ do
            U.modify (siftUp 2) [0 :: Int, 1, 2]
             `shouldBe` [2 :: Int, 1, 0]
        it "siftUp [2, 1, 0] 1 == [2, 1, 0]" $ do
            U.modify (siftUp 1) [2 :: Int, 1, 0]
             `shouldBe` [2 :: Int, 1, 0]
        it "siftUp [2, 1, 0] 2 == [2, 1, 0]" $ do
            U.modify (siftUp 2) [2 :: Int, 1, 0]
             `shouldBe` [2 :: Int, 1, 0]
    describe "siftDown" $ do
        it "siftDown [0, 1, 2] 0 == [2, 1, 0]" $ do
            U.modify (siftDown 0) [0 :: Int, 1, 2]
             `shouldBe` [2 :: Int, 1, 0]
        it "siftDown [0, 2, 1] 0 == [2, 1, 0]" $ do
            U.modify (siftDown 0) [2 :: Int, 1, 0]
             `shouldBe` [2 :: Int, 1, 0]
        it "siftDown [1, 0, 2] 0 == [2, 0, 1]" $ do
            U.modify (siftDown 0) [1 :: Int, 0, 2]
             `shouldBe` [2 :: Int, 0, 1]
        it "siftDown [1, 2, 0] 0 == [2, 1, 0]" $ do
            U.modify (siftDown 0) [1 :: Int, 2, 0]
             `shouldBe` [2 :: Int, 1, 0]
        it "siftDown [2, 0, 1] 0 == [2, 0, 1]" $ do
            U.modify (siftDown 0) [2 :: Int, 0, 1]
             `shouldBe` [2 :: Int, 0, 1]
        it "siftDown [2, 1, 0] 0 == [2, 1, 0]" $ do
            U.modify (siftDown 0) [2 :: Int, 1, 0]
             `shouldBe` [2 :: Int, 1, 0]
    prop "heap sort naive" prop_heapSortNaive

prop_heapSortNaive :: [Int] -> Property
prop_heapSortNaive xs = monadicIO $ do
    sorted <- run $ do
        h <- buildBinaryHeap $ U.fromList xs
        U.toList <$> U.replicateM (length xs)
            (fromJust <$> deleteFindMaxBH h)
    assert $ L.sortBy (flip compare) xs == sorted
