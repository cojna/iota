{-# LANGUAGE OverloadedLists #-}

module Data.Heap.PairingHeap.MinSpec where

import           Data.Heap.PairingHeap.Min
import qualified Data.List                 as L
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec = do
    describe "deleteFindMinPH" $ do
        it "deleteFindMinPH [1, 2] == Just (1, [2])" $ do
            deleteFindMinPH [1, 2] `shouldBe` Just (1, [2])
        it "deleteFindMinPH [2, 1] == Just (1, [2])" $ do
            deleteFindMinPH [2, 1] `shouldBe` Just (1, [2])
        it "deleteFindMinPH [1, 1] == Just (1, [1])" $ do
            deleteFindMinPH [1, 1] `shouldBe` Just (1, [1])
        it "deleteFindMinPH [1] == Just (1, MinEmpty)" $ do
            deleteFindMinPH [1] `shouldBe` Just (1, MinEmpty)
        it "deleteFindMinPH [] == Nothing" $ do
            deleteFindMinPH ([] :: MinHeap Int) `shouldBe` Nothing
    prop "heap sort" prop_priority

prop_priority :: [Int] -> Bool
prop_priority xs = L.sort xs == toList h
  where
    h = foldr insertMinPH emptyMinPH xs
