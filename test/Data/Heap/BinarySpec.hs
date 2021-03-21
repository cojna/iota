{-# LANGUAGE OverloadedLists #-}

module Data.Heap.BinarySpec (main, spec) where

import Control.Monad.Primitive
import Data.Functor.Identity
import Data.Heap.Binary
import Data.Ord
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MinBinaryHeap" $ do
    let newMinBinaryHeapInt :: (PrimMonad m) => Int -> m (MinBinaryHeap (PrimState m) Int)
        newMinBinaryHeapInt = newMinBinaryHeap
    it "new; size; #=> 0" $ do
      h <- newMinBinaryHeapInt 10
      getBinaryHeapSize h `shouldReturn` 0
    it "new; insert 1; size; #=> 1" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 1 h
      getBinaryHeapSize h `shouldReturn` 1
    it "new; insert 1; delete; size; #=> 0" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 1 h
      unsafeDeleteBH h
      getBinaryHeapSize h `shouldReturn` 0
    it "new; view; #=> Nothing" $ do
      h <- newMinBinaryHeapInt 10
      viewBH h `shouldReturn` Nothing
    it "new; insert 1; view; #=> Just 1" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 1 h
      viewBH h `shouldReturn` Just 1
    it "new; insert 1; insert 2; view; #=> Just 1" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 1 h
      insertBH 2 h
      viewBH h `shouldReturn` Just 1
    it "new; insert 2; insert 1; view; #=> Just 1" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 2 h
      insertBH 1 h
      viewBH h `shouldReturn` Just 1
    it "new; insert 1; insert 2; delete; view #=> Just 2" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 1 h
      insertBH 2 h
      unsafeDeleteBH h
      viewBH h `shouldReturn` Just 2
    it "new; insert 2; insert 1; modify negate; view; #=> Just (-1)" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 2 h
      insertBH 1 h
      modifyTopBH negate h
      viewBH h `shouldReturn` Just (-1)
    it "new; insert 2; insert 1; modify (+100); view; #=> Just 2" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 2 h
      insertBH 1 h
      modifyTopBH (+ 100) h
      viewBH h `shouldReturn` Just 2
    it "new; insert 1; insert 2; clear; view #=> Nothing" $ do
      h <- newMinBinaryHeapInt 10
      insertBH 1 h
      insertBH 2 h
      clearBH h
      viewBH h `shouldReturn` Nothing

    let siftUp
          , siftDown ::
            (PrimMonad m) =>
            Int ->
            UM.MVector (PrimState m) Int ->
            m ()
        siftUp = siftUpBy (compareVia Identity)
        siftDown = siftDownBy (compareVia Identity)

    describe "siftUp" $ do
      it "siftUp [0, 1, 2] 1 == [0, 1, 2]" $ do
        U.modify (siftUp 1) [0, 1, 2]
          `shouldBe` [0, 1, 2]
      it "siftUp [0, 1, 2] 2 == [0, 1, 2]" $ do
        U.modify (siftUp 2) [0, 1, 2]
          `shouldBe` [0, 1, 2]
      it "siftUp [2, 1, 0] 1 == [1, 2, 0]" $ do
        U.modify (siftUp 1) [2, 1, 0]
          `shouldBe` [1, 2, 0]
      it "siftUp [2, 1, 0] 2 == [0, 1, 2]" $ do
        U.modify (siftUp 2) [2, 1, 0]
          `shouldBe` [0, 1, 2]
    describe "siftDown" $ do
      it "siftDown [0, 1, 2] 0 == [0, 1, 2]" $ do
        U.modify (siftDown 0) [0, 1, 2]
          `shouldBe` [0, 1, 2]
      it "siftDown [0, 2, 1] 0 == [0, 1, 2]" $ do
        U.modify (siftDown 0) [2, 1, 0]
          `shouldBe` [0, 1, 2]
      it "siftDown [1, 0, 2] 0 == [0, 1, 2]" $ do
        U.modify (siftDown 0) [0, 1, 2]
          `shouldBe` [0, 1, 2]
      it "siftDown [1, 2, 0] 0 == [0, 2, 1]" $ do
        U.modify (siftDown 0) [1, 2, 0]
          `shouldBe` [0, 2, 1]
      it "siftDown [2, 0, 1] 0 == [0, 2, 1]" $ do
        U.modify (siftDown 0) [2, 0, 1]
          `shouldBe` [0, 2, 1]
      it "siftDown [2, 1, 0] 0 == [0, 1, 2]" $ do
        U.modify (siftDown 0) [2, 1, 0]
          `shouldBe` [0, 1, 2]

  describe "MaxBinaryHeap" $ do
    let newMaxBinaryHeapInt :: (PrimMonad m) => Int -> m (MaxBinaryHeap (PrimState m) Int)
        newMaxBinaryHeapInt = newMaxBinaryHeap
    it "new; size; #=> 0" $ do
      h <- newMaxBinaryHeapInt 10
      getBinaryHeapSize h `shouldReturn` 0
    it "new; insert 1; size; #=> 1" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 1 h
      getBinaryHeapSize h `shouldReturn` 1
    it "new; insert 1; delete; size; #=> 0" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 1 h
      unsafeDeleteBH h
      getBinaryHeapSize h `shouldReturn` 0
    it "new; view; #=> Nothing" $ do
      h <- newMaxBinaryHeapInt 10
      viewBH h `shouldReturn` Nothing
    it "new; insert 1; view; #=> Just 1" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 1 h
      viewBH h `shouldReturn` Just 1
    it "new; insert 1; insert 2; view; #=> Just 2" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 1 h
      insertBH 2 h
      viewBH h `shouldReturn` Just 2
    it "new; insert 2; insert 1; view; #=> Just 2" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 2 h
      insertBH 1 h
      viewBH h `shouldReturn` Just 2
    it "new; insert 1; insert 2; delete; view #=> Just 1" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 1 h
      insertBH 2 h
      unsafeDeleteBH h
      viewBH h `shouldReturn` Just 1
    it "new; insert 2; insert 1; modify negate; view; #=> Just 1" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 2 h
      insertBH 1 h
      modifyTopBH negate h
      viewBH h `shouldReturn` Just 1
    it "new; insert 2; insert 1; modify (+100); view; #=> Just 102" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 2 h
      insertBH 1 h
      modifyTopBH (+ 100) h
      viewBH h `shouldReturn` Just 102
    it "new; insert 1; insert 2; clear; view #=> Nothing" $ do
      h <- newMaxBinaryHeapInt 10
      insertBH 1 h
      insertBH 2 h
      clearBH h
      viewBH h `shouldReturn` Nothing

    let siftUp
          , siftDown ::
            (PrimMonad m) =>
            Int ->
            UM.MVector (PrimState m) Int ->
            m ()
        siftUp = siftUpBy (compareVia Down)
        siftDown = siftDownBy (compareVia Down)
    describe "siftUp" $ do
      it "siftUp [0, 1, 2] 1 == [1, 0, 2]" $ do
        U.modify (siftUp 1) [0, 1, 2]
          `shouldBe` [1, 0, 2]
      it "siftUp [0, 1, 2] 2 == [2, 1, 0]" $ do
        U.modify (siftUp 2) [0, 1, 2]
          `shouldBe` [2, 1, 0]
      it "siftUp [2, 1, 0] 1 == [2, 1, 0]" $ do
        U.modify (siftUp 1) [2, 1, 0]
          `shouldBe` [2, 1, 0]
      it "siftUp [2, 1, 0] 2 == [2, 1, 0]" $ do
        U.modify (siftUp 2) [2, 1, 0]
          `shouldBe` [2, 1, 0]
    describe "siftDown" $ do
      it "siftDown [0, 1, 2] 0 == [2, 1, 0]" $ do
        U.modify (siftDown 0) [0, 1, 2]
          `shouldBe` [2, 1, 0]
      it "siftDown [0, 2, 1] 0 == [2, 1, 0]" $ do
        U.modify (siftDown 0) [2, 1, 0]
          `shouldBe` [2, 1, 0]
      it "siftDown [1, 0, 2] 0 == [2, 0, 1]" $ do
        U.modify (siftDown 0) [1, 0, 2]
          `shouldBe` [2, 0, 1]
      it "siftDown [1, 2, 0] 0 == [2, 1, 0]" $ do
        U.modify (siftDown 0) [1, 2, 0]
          `shouldBe` [2, 1, 0]
      it "siftDown [2, 0, 1] 0 == [2, 0, 1]" $ do
        U.modify (siftDown 0) [2, 0, 1]
          `shouldBe` [2, 0, 1]
      it "siftDown [2, 1, 0] 0 == [2, 1, 0]" $ do
        U.modify (siftDown 0) [2, 1, 0]
          `shouldBe` [2, 1, 0]
