module Data.Heap.PairingHeap.MinSpec where

import qualified Data.List                 as L
import           Data.Heap.PairingHeap.Min
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec =
    describe "MinHeap" $
        prop "heap sort" prop_priority

prop_priority :: [Int] -> Bool
prop_priority xs = L.sort xs == toList h
  where
    h = foldr _Hinsert _Hempty xs
