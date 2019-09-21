{-# LANGUAGE CPP, TypeFamilies #-}

module Data.Heap.PairingHeap.Max where

import           Data.Function
import qualified Data.List      as L
import           Data.Monoid
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
import           Data.Semigroup as Semigroup
#endif
import           GHC.Exts

data MaxHeap a = MaxFork !a [MaxHeap a] | MaxEmpty

emptyMaxPH :: MaxHeap a
emptyMaxPH = MaxEmpty
{-# INLINE emptyMaxPH #-}

singletonMaxPH :: a -> MaxHeap a
singletonMaxPH = flip MaxFork []
{-# INLINE singletonMaxPH #-}

nullMaxPH :: MaxHeap a -> Bool
nullMaxPH (MaxFork _ _) = False
nullMaxPH MaxEmpty      = True
{-# INLINE nullMaxPH #-}

insertMaxPH :: Ord a => a -> MaxHeap a -> MaxHeap a
insertMaxPH = mergeMaxPH . singletonMaxPH
{-# INLINE insertMaxPH #-}

maxElemPH :: MaxHeap a -> Maybe a
maxElemPH (MaxFork x _) = Just x
maxElemPH MaxEmpty      = Nothing
{-# INLINE maxElemPH #-}

deleteMaxPH :: Ord a => MaxHeap a -> Maybe (MaxHeap a)
deleteMaxPH (MaxFork _ hs) = Just $! mergePairsMaxPH hs
deleteMaxPH MaxEmpty       = Nothing
{-# INLINE deleteMaxPH #-}

deleteFindMaxPH :: Ord a => MaxHeap a -> Maybe (a, MaxHeap a)
deleteFindMaxPH (MaxFork x hs) = case mergePairsMaxPH hs of
    merged -> Just $! (x, merged)
deleteFindMaxPH MaxEmpty       = Nothing
{-# INLINE deleteFindMaxPH #-}

mergeMaxPH :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a
mergeMaxPH hx@(MaxFork x hxs) hy@(MaxFork y hys)
  | y <= x    = MaxFork x (hy:hxs)
  | otherwise = MaxFork y (hx:hys)
mergeMaxPH MaxEmpty hy = hy
mergeMaxPH hx MaxEmpty = hx
{-# INLINE mergeMaxPH #-}

mergePairsMaxPH :: Ord a => [MaxHeap a] -> MaxHeap a
mergePairsMaxPH = mconcat . mergePairs
  where
    mergePairs (x:y:xs) = case x <> y of
        merged -> merged : mergePairs xs
    mergePairs xs = xs
{-# INLINE mergePairsMaxPH #-}

instance Ord a => Eq (MaxHeap a) where
    (==) = (==) `on` toList

instance Ord a => Ord (MaxHeap a) where
    compare = compare `on` toList

instance Ord a => IsList (MaxHeap a) where
    type Item (MaxHeap a) = a
    fromList = mergePairsMaxPH . map singletonMaxPH
    toList = L.unfoldr deleteFindMaxPH

instance (Show a, Ord a) => Show (MaxHeap a) where
    show = show . toList

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
instance Ord a => Semigroup.Semigroup (MaxHeap a) where
  (<>) = mergeMaxPH
#endif

instance Ord a => Monoid (MaxHeap a) where
    mempty = emptyMaxPH
    {-# INLINE mempty #-}
#if MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
#elif MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = mergeMaxPH
    {-# INLINE mappend #-}
#endif


