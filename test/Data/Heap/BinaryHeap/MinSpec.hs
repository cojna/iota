{-# LANGUAGE OverloadedLists #-}

module Data.Heap.BinaryHeap.MinSpec where

import           Control.Monad
import           Data.Function
import           Data.Heap.BinaryHeap.Min
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
        it "siftUp [0, 1, 2] 1 == [0, 1, 2]" $ do
            U.modify (siftUp 1) [0 :: Int, 1, 2]
             `shouldBe` [0 :: Int, 1, 2]
        it "siftUp [0, 1, 2] 2 == [0, 1, 2]" $ do
            U.modify (siftUp 2) [0 :: Int, 1, 2]
             `shouldBe` [0 :: Int, 1, 2]
        it "siftUp [2, 1, 0] 1 == [1, 2, 0]" $ do
            U.modify (siftUp 1) [2 :: Int, 1, 0]
             `shouldBe` [1 :: Int, 2, 0]
        it "siftUp [2, 1, 0] 2 == [0, 1, 2]" $ do
            U.modify (siftUp 2) [2 :: Int, 1, 0]
             `shouldBe` [0 :: Int, 1, 2]
    describe "siftDown" $ do
        it "siftDown [0, 1, 2] 0 == [0, 1, 2]" $ do
            U.modify (siftDown 0) [0 :: Int, 1, 2]
             `shouldBe` [0 :: Int, 1, 2]
        it "siftDown [0, 2, 1] 0 == [0, 1, 2]" $ do
            U.modify (siftDown 0) [2 :: Int, 1, 0]
             `shouldBe` [0 :: Int, 1, 2]
        it "siftDown [1, 0, 2] 0 == [0, 1, 2]" $ do
            U.modify (siftDown 0) [0 :: Int, 1, 2]
             `shouldBe` [0 :: Int, 1, 2]
        it "siftDown [1, 2, 0] 0 == [0, 2, 1]" $ do
            U.modify (siftDown 0) [1 :: Int, 2, 0]
             `shouldBe` [0 :: Int, 2, 1]
        it "siftDoen [2, 0, 1] 0 == [0, 2, 1]" $ do
            U.modify (siftDown 0) [2 :: Int, 0, 1]
             `shouldBe` [0 :: Int, 2, 1]
        it "siftDoen [2, 1, 0] 0 == [0, 1, 2]" $ do
            U.modify (siftDown 0) [2 :: Int, 1, 0]
             `shouldBe` [0 :: Int, 1, 2]
    prop "heap sort naive" prop_heapSortNaive

prop_heapSortNaive :: [Int] -> Property
prop_heapSortNaive xs = monadicIO $ do
    sorted <- run $ do
        h <- buildBinaryHeap $ U.fromList xs
        U.toList <$> U.replicateM (length xs)
            (fromJust <$> deleteFindMinBH h)
    assert $ L.sort xs == sorted
