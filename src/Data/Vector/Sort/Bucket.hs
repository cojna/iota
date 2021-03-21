module Data.Vector.Sort.Bucket where

import qualified Data.Vector.Unboxed as U

bucketSort :: Int -> U.Vector Int -> U.Vector Int
bucketSort bucketSize =
  U.concatMap (uncurry $ flip U.replicate)
    . U.indexed
    . U.unsafeAccumulate (+) (U.replicate bucketSize 0)
    . U.map (flip (,) 1)
{-# INLINE bucketSort #-}
