{-# LANGUAGE ViewPatterns #-}

module Data.Vector.SortSpec where

import qualified Data.List                 as L
import           Data.Proxy
import           Data.Vector.Sort
import qualified Data.Vector.Unboxed       as U
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
--
import           Data.Word64

spec :: Spec
spec = do
    describe "buckerSort" $
        prop "naive" prop_bucketSortNaive
    describe "radixSort32" $
        prop "naive" prop_radixSort32Naive
    describe "radixSort64" $
        prop "naive" prop_radixSort64Naive
    describe "radixSort/Int" $
        prop "naive" $ prop_radixSortNaive (Proxy :: Proxy Int)
    describe "radixSort/(Int, Int)" $
        prop "naive" $ prop_radixSortNaive (Proxy :: Proxy (Int, Int))
    describe "radixSort/(Int, Int, Int)" $
        prop "naive" $ prop_radixSortNaive (Proxy :: Proxy (Int, Int, Int))
    describe "radixSortNonNegative/Int" $
        prop "naive" $ prop_radixSortNonNegativeNaiveInt
    describe "radixSortNonNegative/(Int, Int)" $
        prop "naive" $ prop_radixSortNonNegativeNaiveIntInt
    describe "radixSortNonNegative/(Int, Int, Int)" $
        prop "naive" $ prop_radixSortNonNegativeNaiveIntIntInt

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

prop_radixSortNaive
    :: (Ord a, U.Unbox a, Word64Encode a) => Proxy a -> [a] -> Bool
prop_radixSortNaive _ xs
    = radixSort (U.fromList xs) == U.fromList (L.sort xs)

prop_radixSortNonNegativeNaiveInt :: [NonNegative Int] -> Bool
prop_radixSortNonNegativeNaiveInt (map getNonNegative -> xs)
    = radixSort (U.fromList xs) == U.fromList (L.sort xs)

prop_radixSortNonNegativeNaiveIntInt :: [(NonNegative Int, NonNegative Int)] -> Bool
prop_radixSortNonNegativeNaiveIntInt
    (map (\(x, y) -> (getNonNegative x, getNonNegative y)) -> xs)
    = radixSort (U.fromList xs) == U.fromList (L.sort xs)

prop_radixSortNonNegativeNaiveIntIntInt :: [(NonNegative Int, NonNegative Int, NonNegative Int)] -> Bool
prop_radixSortNonNegativeNaiveIntIntInt
    (map (\(x, y, z) -> (getNonNegative x, getNonNegative y, getNonNegative z)) -> xs)
    = radixSort (U.fromList xs) == U.fromList (L.sort xs)

