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
    concatMap (\(k, x) -> replicate x k)
      . IM.toList
      . (coerce :: IntHeap -> IM.IntMap Int)

emptyIH :: IntHeap
emptyIH = coerce (IM.empty :: IM.IntMap Int)

singletonIH :: Int -> IntHeap
singletonIH x = coerce $ IM.singleton x (1 :: Int)

-- | /O(min(n,W))/
insertIH :: Int -> IntHeap -> IntHeap
insertIH x = coerce . IM.insertWith (+) x (1 :: Int) . coerce

-- | /O(min(n,W))/
deleteIH :: Int -> IntHeap -> IntHeap
deleteIH x =
  (coerce :: IM.IntMap Int -> IntHeap)
    . IM.update (\y -> if y > 1 then Just $! y - 1 else Nothing) x
    . coerce

-- | /O(min(n,W))/
deleteAllIH :: Int -> IntHeap -> IntHeap
deleteAllIH x =
  (coerce :: IM.IntMap Int -> IntHeap)
    . IM.delete x
    . coerce

-- | /O(min(n,W))/
memberIH :: Int -> IntHeap -> Bool
memberIH x = IM.member x . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(min(n,W))/
notMemberIH :: Int -> IntHeap -> Bool
notMemberIH x = not . memberIH x

-- | /P(min(n, W))/
countIH :: Int -> IntHeap -> Int
countIH key = IM.findWithDefault 0 key . coerce

-- | /O(log n)/
lookupLTIH :: Int -> IntHeap -> Maybe Int
lookupLTIH x =
  fmap fst
    . IM.lookupLT x
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(log n)/
lookupGTIH :: Int -> IntHeap -> Maybe Int
lookupGTIH x =
  fmap fst
    . IM.lookupGT x
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(log n)/
lookupLEIH :: Int -> IntHeap -> Maybe Int
lookupLEIH x =
  fmap fst
    . IM.lookupLE x
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(log n)/
lookupGEIH :: Int -> IntHeap -> Maybe Int
lookupGEIH x =
  fmap fst
    . IM.lookupGE x
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(1)/
nullIH :: IntHeap -> Bool
nullIH =
  IM.null
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(n)/
sizeIH :: IntHeap -> Int
sizeIH =
  IM.foldl' (+) 0
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(min(n,W))/
findMinIH :: IntHeap -> Int
findMinIH =
  fst
    . IM.findMin
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(min(n,W))/
findMaxIH :: IntHeap -> Int
findMaxIH =
  fst
    . IM.findMax
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(min(n,W))/
deleteMinIH :: IntHeap -> IntHeap
deleteMinIH =
  coerce
    . IM.updateMin (\x -> if x > 1 then Just $! x - 1 else Nothing)
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(min(n,W))/
deleteMaxIH :: IntHeap -> IntHeap
deleteMaxIH =
  coerce
    . IM.updateMax (\x -> if x > 1 then Just $! x - 1 else Nothing)
    . (coerce :: IntHeap -> IM.IntMap Int)

-- | /O(min(n,W))/
maxViewIH :: IntHeap -> Maybe (Int, IntHeap)
maxViewIH =
  maybe Nothing just
    . IM.maxViewWithKey
    . (coerce :: IntHeap -> IM.IntMap Int)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntHeap)
    just ((k, x), m)
      | x > 1 = case IM.insert k (x - 1) m of
          m' -> Just (k, coerce m')
      | otherwise = Just (k, coerce m)

-- | /O(min(n,W))/
minViewIH :: IntHeap -> Maybe (Int, IntHeap)
minViewIH =
  maybe Nothing just
    . IM.minViewWithKey
    . (coerce :: IntHeap -> IM.IntMap Int)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntHeap)
    just ((k, x), m)
      | x > 1 = case IM.insert k (x - 1) m of
          m' -> Just (k, coerce m')
      | otherwise = Just (k, coerce m)
