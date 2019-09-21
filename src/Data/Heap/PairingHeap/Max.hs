{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Heap.PairingHeap.Max where

import           Data.Function
import qualified Data.List     as L
import           Data.Monoid
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
import           Data.Semigroup as Semigroup
#endif
import           GHC.Exts

data MaxHeap a = MaxFork !a [MaxHeap a] | MaxEmpty

_HHempty :: MaxHeap a
_HHempty = MaxEmpty
{-# INLINE _HHempty #-}

_HHsingleton :: a -> MaxHeap a
_HHsingleton x = MaxFork x []
{-# INLINE _HHsingleton #-}

_HHnull :: MaxHeap a -> Bool
_HHnull MaxEmpty      = True
_HHnull (MaxFork _ _) = False
{-# INLINE _HHnull #-}

_HHinsert :: Ord a => a -> MaxHeap a -> MaxHeap a
_HHinsert x = _HHmerge (MaxFork x [])
{-# INLINE _HHinsert #-}

_HHMaxElem :: MaxHeap a -> Maybe a
_HHMaxElem (MaxFork x _) = Just x
_HHMaxElem MaxEmpty      = Nothing
{-# INLINE _HHMaxElem #-}

_HHdeleteMax :: Ord a => MaxHeap a -> Maybe (MaxHeap a)
_HHdeleteMax (MaxFork _ hs) = Just $ _HHmergePairs hs
_HHdeleteMax MaxEmpty       = Nothing
{-# INLINE _HHdeleteMax #-}

_HHdeleteFindMax :: Ord a => MaxHeap a -> Maybe (a, MaxHeap a)
_HHdeleteFindMax (MaxFork x hs) = Just (x, _HHmergePairs hs)
_HHdeleteFindMax MaxEmpty       = Nothing
{-# INLINE _HHdeleteFindMax #-}

_HHmerge :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a
_HHmerge hx@(MaxFork x hxs) hy@(MaxFork y hys)
  | y <= x    = MaxFork x (hy:hxs)
  | otherwise = MaxFork y (hx:hys)
_HHmerge MaxEmpty hy = hy
_HHmerge hx _ = hx
{-# INLINE _HHmerge #-}

_HHmergePairs :: Ord a => [MaxHeap a] -> MaxHeap a
_HHmergePairs (x:y:hs) = (x <> y) <> _HHmergePairs hs
_HHmergePairs [x]      = x
_HHmergePairs []       = MaxEmpty
{-# INLINE _HHmergePairs #-}

instance Ord a => Eq (MaxHeap a) where
    (==) = (==) `on` toList

instance Ord a => Ord (MaxHeap a) where
    compare = compare `on` toList

instance Ord a => IsList (MaxHeap a) where
    type Item (MaxHeap a) = a
    fromList xs = _HHmergePairs $ map _HHsingleton xs
    toList = L.unfoldr _HHdeleteFindMax

instance (Show a, Ord a) => Show (MaxHeap a) where
    show = show . toList

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
instance Ord a => Semigroup.Semigroup (MaxHeap a) where
  (<>) = _HHmerge
#endif

instance Ord a => Monoid (MaxHeap a) where
    mempty = _HHempty
    {-# INLINE mempty #-}
    mconcat = _HHmergePairs
    {-# INLINE mconcat #-}
#if MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
#elif MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = _HHmerge
    {-# INLINE mappend #-}
#endif


