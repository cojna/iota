module Data.MinHeapSpec where

import qualified Data.List                 as L
import           Data.MinHeap
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
