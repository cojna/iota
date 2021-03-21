{-# LANGUAGE OverloadedLists #-}

module Data.Heap.PairingHeap.MaxSpec (main, spec) where

import Data.Heap.PairingHeap.Max
import qualified Data.List as L
import GHC.Exts
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deleteFindMaxPH" $ do
    it "deleteFindMaxPH [1, 2] == Just (2, [1])" $ do
      deleteFindMaxPH [1, 2] `shouldBe` Just (2, [1])
    it "deleteFindMaxPH [2, 1] == Just (2, [1])" $ do
      deleteFindMaxPH [2, 1] `shouldBe` Just (2, [1])
    it "deleteFindMaxPH [1, 1] == Just (1, [1])" $ do
      deleteFindMaxPH [1, 1] `shouldBe` Just (1, [1])
    it "deleteFindMaxPH [1] == Just (1, MaxEmpty)" $ do
      deleteFindMaxPH [1] `shouldBe` Just (1, MaxEmpty)
    it "deleteFindMaxPH [] == Nothing" $ do
      deleteFindMaxPH ([] :: MaxHeap Int) `shouldBe` Nothing
  prop "heap sort" prop_priority

prop_priority :: [Int] -> Bool
prop_priority xs = L.sortBy (flip compare) xs == toList h
  where
    h = foldr insertMaxPH emptyMaxPH xs
