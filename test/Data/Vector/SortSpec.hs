{-# LANGUAGE ViewPatterns #-}

module Data.Vector.SortSpec where

import qualified Data.List                 as L
import           Data.Vector.Sort
import qualified Data.Vector.Unboxed       as U
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec = do
    describe "buckerSort" $
        prop "naive" prop_bucketSortNaive
    describe "radixSort32" $
        prop "naive" prop_radixSort32Naive
    describe "radixSort64" $
        prop "naive" prop_radixSort64Naive

prop_bucketSortNaive :: [NonNegative Int] -> Bool
prop_bucketSortNaive (map getNonNegative -> xs)
    = bucketSort bucketSize v == U.fromList (L.sort xs)
  where
    v = U.fromList xs
    bucketSize = 1 + U.foldl' max 0 v

prop_radixSort32Naive :: [Word32] -> Bool
prop_radixSort32Naive xs
    = radixSort32 (U.fromList xs) == U.fromList (L.sort xs)

prop_radixSort64Naive :: [Word64] -> Bool
prop_radixSort64Naive xs
    = radixSort64 (U.fromList xs) == U.fromList (L.sort xs)
