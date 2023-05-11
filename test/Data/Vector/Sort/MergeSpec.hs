{-# LANGUAGE ViewPatterns #-}

module Data.Vector.Sort.MergeSpec (main, spec) where

import qualified Data.List as L
import Data.Ord
import Data.Vector.Sort.Merge
import qualified Data.Vector.Unboxed as U
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mergeSort_" $
    prop "naive" prop_mergeSortNaive
  describe "mergeSortBy_" $
    prop "stable" prop_stableSort
  describe "inversionNumber" $
    prop "naive" prop_invNumNaive

prop_mergeSortNaive :: [Int] -> Bool
prop_mergeSortNaive xs =
  U.modify mergeSort_ (U.fromList xs) == U.fromList (L.sort xs)

prop_stableSort :: [Int] -> Bool
prop_stableSort xs =
  U.modify (mergeSortBy_ cmp) (U.fromList ixs)
    == U.fromList (L.sortBy cmp ixs)
  where
    cmp = comparing snd
    ixs :: [(Int, Int)]
    ixs = zip [0 ..] xs

inversionNumberNaive :: U.Vector Int -> Int
inversionNumberNaive xs =
  sum [1 | i <- [0 .. n -1], j <- [i + 1 .. n -1], xs U.! i > xs U.! j]
  where
    n = U.length xs

prop_invNumNaive :: [Int] -> Bool
prop_invNumNaive (U.fromList -> xs) =
  inversionNumber xs == inversionNumberNaive xs