{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.IntMultiSet where

import Data.Coerce
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import GHC.Exts

newtype IntMultiSet = IntMultiSet {getIntMultiSet :: IM.IntMap Int}
  deriving (Eq)

instance Show IntMultiSet where
  show = show . toList

instance IsList IntMultiSet where
  type Item IntMultiSet = Int
  fromList = L.foldl' (flip insertIMS) emptyIMS
  toList =
    concatMap (\(k, x) -> replicate x k)
      . IM.toList
      . (coerce :: IntMultiSet -> IM.IntMap Int)

emptyIMS :: IntMultiSet
emptyIMS = coerce (IM.empty :: IM.IntMap Int)

singletonIMS :: Int -> IntMultiSet
singletonIMS x = coerce $ IM.singleton x (1 :: Int)

-- | /O(min(n,W))/
insertIMS :: Int -> IntMultiSet -> IntMultiSet
insertIMS x = coerce . IM.insertWith (+) x (1 :: Int) . coerce

-- | /O(min(n,W))/
deleteIMS :: Int -> IntMultiSet -> IntMultiSet
deleteIMS x =
  (coerce :: IM.IntMap Int -> IntMultiSet)
    . IM.update (\y -> if y > 1 then Just $! y - 1 else Nothing) x
    . coerce

-- | /O(min(n,W))/
deleteAllIMS :: Int -> IntMultiSet -> IntMultiSet
deleteAllIMS x =
  (coerce :: IM.IntMap Int -> IntMultiSet)
    . IM.delete x
    . coerce

-- | /O(min(n,W))/
memberIMS :: Int -> IntMultiSet -> Bool
memberIMS x = IM.member x . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
notMemberIMS :: Int -> IntMultiSet -> Bool
notMemberIMS x = not . memberIMS x

-- | /P(min(n, W))/
countIMS :: Int -> IntMultiSet -> Int
countIMS key = IM.findWithDefault 0 key . coerce

-- | /O(log n)/
lookupLTIMS :: Int -> IntMultiSet -> Maybe Int
lookupLTIMS x =
  fmap fst . IM.lookupLT x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(log n)/
lookupGTIMS :: Int -> IntMultiSet -> Maybe Int
lookupGTIMS x =
  fmap fst . IM.lookupGT x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(log n)/
lookupLEIMS :: Int -> IntMultiSet -> Maybe Int
lookupLEIMS x =
  fmap fst . IM.lookupLE x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(log n)/
lookupGEIMS :: Int -> IntMultiSet -> Maybe Int
lookupGEIMS x =
  fmap fst . IM.lookupGE x
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(1)/
nullIMS :: IntMultiSet -> Bool
nullIMS =
  IM.null
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(n)/
sizeIMS :: IntMultiSet -> Int
sizeIMS =
  IM.foldl' (+) 0
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
findMinIMS :: IntMultiSet -> Int
findMinIMS =
  fst
    . IM.findMin
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
findMaxIMS :: IntMultiSet -> Int
findMaxIMS =
  fst
    . IM.findMax
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
deleteMinIMS :: IntMultiSet -> IntMultiSet
deleteMinIMS =
  coerce
    . IM.updateMin (\x -> if x > 1 then Just $! x - 1 else Nothing)
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
deleteMaxIMS :: IntMultiSet -> IntMultiSet
deleteMaxIMS =
  coerce
    . IM.updateMax (\x -> if x > 1 then Just $! x - 1 else Nothing)
    . (coerce :: IntMultiSet -> IM.IntMap Int)

-- | /O(min(n,W))/
maxViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
maxViewIMS =
  maybe Nothing just
    . IM.maxViewWithKey
    . (coerce :: IntMultiSet -> IM.IntMap Int)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet)
    just ((k, x), m)
      | x > 1 = case IM.insert k (x - 1) m of
        m' -> Just (k, coerce m')
      | otherwise = Just (k, coerce m)

-- | /O(min(n,W))/
minViewIMS :: IntMultiSet -> Maybe (Int, IntMultiSet)
minViewIMS =
  maybe Nothing just
    . IM.minViewWithKey
    . (coerce :: IntMultiSet -> IM.IntMap Int)
  where
    just :: ((Int, Int), IM.IntMap Int) -> Maybe (Int, IntMultiSet)
    just ((k, x), m)
      | x > 1 = case IM.insert k (x - 1) m of
        m' -> Just (k, coerce m')
      | otherwise = Just (k, coerce m)
