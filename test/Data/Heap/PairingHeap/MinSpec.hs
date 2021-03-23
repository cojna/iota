{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Data.Heap.PairingHeap.MinSpec (main, spec) where

import Data.Heap.PairingHeap.Min
import qualified Data.List as L
import GHC.Exts
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deleteFindMinPH" $ do
    it "deleteFindMinPH [1, 2] == Just (1, [2])" $ do
      deleteFindMinPH @Int [1, 2] `shouldBe` Just (1, [2])
    it "deleteFindMinPH [2, 1] == Just (1, [2])" $ do
      deleteFindMinPH @Int [2, 1] `shouldBe` Just (1, [2])
    it "deleteFindMinPH [1, 1] == Just (1, [1])" $ do
      deleteFindMinPH @Int [1, 1] `shouldBe` Just (1, [1])
    it "deleteFindMinPH [1] == Just (1, MinEmpty)" $ do
      deleteFindMinPH @Int [1] `shouldBe` Just (1, MinEmpty)
    it "deleteFindMinPH [] == Nothing" $ do
      deleteFindMinPH @Int ([] :: MinHeap Int) `shouldBe` Nothing
  prop "heap sort" prop_priority

prop_priority :: [Int] -> Bool
prop_priority xs = L.sort xs == toList h
  where
    h = foldr insertMinPH emptyMinPH xs
