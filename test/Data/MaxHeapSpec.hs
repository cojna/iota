module Data.MaxHeapSpec where

import qualified Data.List                 as L
import           Data.MaxHeap
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec =
    describe "MaxHeap" $
        prop "heap sort" prop_priority

prop_priority :: [Int] -> Bool
prop_priority xs = L.sortBy (flip compare) xs == toList h
  where
    h = foldr _HHinsert _HHempty xs