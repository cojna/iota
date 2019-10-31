{-# LANGUAGE DataKinds, ViewPatterns #-}

module Data.Vector.Sort.BucketSpec (main, spec) where

import qualified Data.List               as L
import           Data.Vector.Sort.Bucket
import qualified Data.Vector.Unboxed     as U
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "buckerSort" $
        prop "naive" prop_bucketSortNaive

prop_bucketSortNaive :: [Modulo 256 Int] -> Bool
prop_bucketSortNaive (map getModulo -> xs)
    = bucketSort bucketSize v == U.fromList (L.sort xs)
  where
    v = U.fromList xs
    bucketSize = 1 + U.foldl' max 0 v
