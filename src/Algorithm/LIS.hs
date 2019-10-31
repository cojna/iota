module Algorithm.LIS where

import qualified Data.IntSet         as IS
import qualified Data.Vector.Unboxed as U

longestIncreasingSubsequence :: U.Vector Int -> Int
longestIncreasingSubsequence xs = IS.size $ U.foldl' step IS.empty xs
  where
    step s x = case IS.lookupGE x s of
        Just y | x < y     -> IS.insert x $ IS.delete y s
               | otherwise -> s
        Nothing            -> IS.insert x s
