{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Heap.PairingHeap.Min where

import Data.Function
import qualified Data.List as L
import GHC.Exts

data MinHeap a = MinFork !a [MinHeap a] | MinEmpty

emptyMinPH :: MinHeap a
emptyMinPH = MinEmpty
{-# INLINE emptyMinPH #-}

singletonMinPH :: a -> MinHeap a
singletonMinPH = flip MinFork []
{-# INLINE singletonMinPH #-}

nullMinPH :: MinHeap a -> Bool
nullMinPH (MinFork _ _) = False
nullMinPH MinEmpty = True
{-# INLINE nullMinPH #-}

insertMinPH :: Ord a => a -> MinHeap a -> MinHeap a
insertMinPH = mergeMinPH . singletonMinPH
{-# INLINE insertMinPH #-}

minElemPH :: MinHeap a -> Maybe a
minElemPH (MinFork x _) = Just x
minElemPH MinEmpty = Nothing
{-# INLINE minElemPH #-}

deleteMinPH :: Ord a => MinHeap a -> Maybe (MinHeap a)
deleteMinPH (MinFork _ hs) = Just $! mergePairsMinPH hs
deleteMinPH MinEmpty = Nothing
{-# INLINE deleteMinPH #-}

deleteFindMinPH :: Ord a => MinHeap a -> Maybe (a, MinHeap a)
deleteFindMinPH (MinFork x hs) = case mergePairsMinPH hs of
  merged -> Just $! (x, merged)
deleteFindMinPH MinEmpty = Nothing
{-# INLINE deleteFindMinPH #-}

mergeMinPH :: Ord a => MinHeap a -> MinHeap a -> MinHeap a
mergeMinPH hx@(MinFork x hxs) hy@(MinFork y hys)
  | x <= y = MinFork x (hy : hxs)
  | otherwise = MinFork y (hx : hys)
mergeMinPH MinEmpty hy = hy
mergeMinPH hx MinEmpty = hx
{-# INLINE mergeMinPH #-}

mergePairsMinPH :: Ord a => [MinHeap a] -> MinHeap a
mergePairsMinPH = mconcat . mergePairs
  where
    mergePairs (x : y : xs) = case x <> y of
      merged -> merged : mergePairs xs
    mergePairs xs = xs
{-# INLINE mergePairsMinPH #-}

instance Ord a => Eq (MinHeap a) where
  (==) = (==) `on` toList

instance Ord a => Ord (MinHeap a) where
  compare = compare `on` toList

instance Ord a => IsList (MinHeap a) where
  type Item (MinHeap a) = a
  fromList = mergePairsMinPH . map singletonMinPH
  toList = L.unfoldr deleteFindMinPH

instance (Show a, Ord a) => Show (MinHeap a) where
  show = show . toList

instance Ord a => Semigroup (MinHeap a) where
  (<>) = mergeMinPH

instance Ord a => Monoid (MinHeap a) where
  mempty = emptyMinPH
  {-# INLINE mempty #-}

#if !MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
    mappend = mergeMinPH
    {-# INLINE mappend #-}
#endif
