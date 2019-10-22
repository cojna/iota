{-# LANGUAGE OverloadedLists #-}

module Data.Heap.BinarySpec (main, spec) where

import           Control.Monad.Primitive
import           Data.Functor.Identity
import           Data.Heap.Binary
import           Data.Ord
import qualified Data.Vector.Unboxed      as U
import qualified Data.Vector.Unboxed.Mutable      as UM
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "MinBinaryHeap" $ do
        let siftUp, siftDown :: (PrimMonad m, U.Unbox a, Ord a)
                => Int -> UM.MVector (PrimState m) a -> m ()
            siftUp = siftUpBy (compareVia Identity)
            siftDown = siftDownBy (compareVia Identity)
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
            it "siftDown [2, 0, 1] 0 == [0, 2, 1]" $ do
                U.modify (siftDown 0) [2 :: Int, 0, 1]
                 `shouldBe` [0 :: Int, 2, 1]
            it "siftDown [2, 1, 0] 0 == [0, 1, 2]" $ do
                U.modify (siftDown 0) [2 :: Int, 1, 0]
                 `shouldBe` [0 :: Int, 1, 2]
    describe "MaxBinaryHeap" $ do
        let siftUp, siftDown :: (PrimMonad m, U.Unbox a, Ord a)
                => Int -> UM.MVector (PrimState m) a -> m ()
            siftUp = siftUpBy (compareVia Down)
            siftDown = siftDownBy (compareVia Down)
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
