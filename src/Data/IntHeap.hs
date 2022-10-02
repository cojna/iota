{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.IntHeap where

import Data.Coerce
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import GHC.Exts

newtype IntHeap = IntHeap {getIntHeap :: IM.IntMap Int}
  deriving (Eq)

instance Show IntHeap where
  show = show . toList

instance IsList IntHeap where
  type Item IntHeap = Int
  fromList = L.foldl' (flip insertIH) emptyIH
  toList =
    concatMap @[] (\(k, x) -> replicate x k)
      . coerce (IM.toList @Int)

emptyIH :: IntHeap
emptyIH = coerce (IM.empty @Int)

singletonIH :: Int -> IntHeap
singletonIH = coerce (flip (IM.singleton @Int) 1)

-- | /O(min(n,W))/
insertIH :: Int -> IntHeap -> IntHeap
insertIH = coerce (flip (IM.insertWith @Int (+)) 1)
{-# INLINE insertIH #-}

{- | /O(min(n,W))/

>>> deleteIH 0 (fromList [0, 0, 1, 2])
[0,1,2]
>>> deleteIH 3 (fromList [0, 0, 1, 2])
[0,0,1,2]
-}
deleteIH :: Int -> IntHeap -> IntHeap
deleteIH = coerce (IM.update @Int (\y -> if y > 1 then Just $! y - 1 else Nothing))

{- | /O(min(n,W))/

>>> deleteAllIH 0 (fromList [0, 0, 1, 2])
[1,2]
>>> deleteAllIH 3 (fromList [0, 0, 1, 2])
[0,0,1,2]
-}
deleteAllIH :: Int -> IntHeap -> IntHeap
deleteAllIH = coerce (IM.delete @Int)

-- | /O(min(n,W))/
memberIH :: Int -> IntHeap -> Bool
memberIH = coerce (IM.member @Int)

-- | /O(min(n,W))/
notMemberIH :: Int -> IntHeap -> Bool
notMemberIH = coerce (IM.notMember @Int)

{- | /O(min(n, W))/

>>> countIH 0 (fromList [0, 0, 1, 2])
2
>>> countIH 1 (fromList [0, 0, 1, 2])
1
>>> countIH (-1) (fromList [0, 0, 1, 2])
0
-}
countIH :: Int -> IntHeap -> Int
countIH = coerce (IM.findWithDefault @Int 0)

{- | /O(log n)/

>>> lookupLTIH 1 (fromList [0, 0, 1, 1, 2, 2])
Just 0
>>> lookupLTIH 0 (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupLTIH :: Int -> IntHeap -> Maybe Int
lookupLTIH x = coerce (fmap fst . IM.lookupLT @Int x)

{- | /O(log n)/

>>> lookupGTIH 1 (fromList [0, 0, 1, 1, 2, 2])
Just 2
>>> lookupGTIH 2 (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupGTIH :: Int -> IntHeap -> Maybe Int
lookupGTIH x = coerce (fmap fst . IM.lookupGT @Int x)

{- | /O(log n)/

>>> lookupLEIH 1 (fromList [0, 0, 1, 1, 2, 2])
Just 1
>>> lookupLEIH 0 (fromList [0, 0, 1, 1, 2, 2])
Just 0
>>> lookupLEIH (-1) (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupLEIH :: Int -> IntHeap -> Maybe Int
lookupLEIH x = coerce (fmap fst . IM.lookupLE @Int x)

{- | /O(log n)/

>>> lookupGEIH 1 (fromList [0, 0, 1, 1, 2, 2])
Just 1
>>> lookupGEIH 2 (fromList [0, 0, 1, 1, 2, 2])
Just 2
>>> lookupGEIH 3 (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupGEIH :: Int -> IntHeap -> Maybe Int
lookupGEIH x = coerce (fmap fst . IM.lookupGE @Int x)

-- | /O(1)/
nullIH :: IntHeap -> Bool
nullIH = coerce (IM.null @Int)

{- | /O(n)/

>>> sizeIH (fromList [0, 0, 1, 2])
4
>>> sizeIH emptyIH
0
-}
sizeIH :: IntHeap -> Int
sizeIH = coerce (IM.foldl' @Int (+) 0)

{- | /O(min(n,W))/

>>> findMinIH (fromList [0, 0, 1, 2])
0
>>> findMinIH emptyIH
*** Exception: findMin: empty map has no minimal element
-}
findMinIH :: IntHeap -> Int
findMinIH = coerce (fst . IM.findMin @Int)

{- | /O(min(n,W))/

>>> findMaxIH (fromList [0, 0, 1, 2])
2
>>> findMaxIH emptyIH
*** Exception: findMax: empty map has no maximal element
-}
findMaxIH :: IntHeap -> Int
findMaxIH = coerce (fst . IM.findMax @Int)

{- | /O(min(n,W))/

>>> deleteMinIH (fromList [0, 0, 1, 2])
[0,1,2]
>>> deleteMinIH emptyIH
*** Exception: updateMinWithKey Nil
-}
deleteMinIH :: IntHeap -> IntHeap
deleteMinIH =
  coerce (IM.updateMin @Int (\x -> if x > 1 then Just $! x - 1 else Nothing))

{- | /O(min(n,W))/

>>> deleteMaxIH (fromList [0, 1, 2, 2])
[0,1,2]

>>> deleteMaxIH emptyIH
*** Exception: updateMaxWithKey Nil
-}
deleteMaxIH :: IntHeap -> IntHeap
deleteMaxIH =
  coerce (IM.updateMax @Int (\x -> if x > 1 then Just $! x - 1 else Nothing))

{- | /O(min(n,W))/

>>> minViewIH (fromList [0, 0, 1, 2])
Just (0,[0,1,2])

>>> minViewIH emptyIH
Nothing
-}
minViewIH :: IntHeap -> Maybe (Int, IntHeap)
minViewIH = coerce (maybe Nothing just . IM.minViewWithKey)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntHeap)
    just ((k, x), m)
      | x > 1 = case IM.insert k (x - 1) m of
          m' -> coerce (Just (k, m'))
      | otherwise = coerce (Just (k, m))

{- | /O(min(n,W))/

>>> maxViewIH (fromList [0, 1, 2, 2])
Just (2,[0,1,2])

>>> maxViewIH emptyIH
Nothing
-}
maxViewIH :: IntHeap -> Maybe (Int, IntHeap)
maxViewIH = coerce (maybe Nothing just . IM.maxViewWithKey)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntHeap)
    just ((k, x), m)
      | x > 1 = case IM.insert k (x - 1) m of
          m' -> coerce (Just (k, m'))
      | otherwise = coerce (Just (k, m))

{- | /O(min(n,W))/

>>> split 1 (fromList [0, 0, 1, 2])
([0,0],[2])
>>> split 0 (fromList [0, 0, 1, 2])
([],[1,2])
>>> split (-1) (fromList [0, 0, 1, 2])
([],[0,0,1,2])
-}
split :: Int -> IntHeap -> (IntHeap, IntHeap)
split = coerce (IM.split @Int)

{- | /O(min(n,W))/

>>> splitLookup 1 (fromList [0, 0, 1, 2])
([0,0],[1],[2])
>>> splitLookup 0 (fromList [0, 0, 1, 2])
([],[0,0],[1,2])
>>> splitLookup (-1) (fromList [0, 0, 1, 2])
([],[],[0,0,1,2])
-}
splitLookup :: Int -> IntHeap -> (IntHeap, [Int], IntHeap)
splitLookup k h = case coerce (IM.splitLookup @Int) k h of
  (l, Just cnt, r) -> (l, replicate cnt k, r)
  (l, Nothing, r) -> (l, [], r)
