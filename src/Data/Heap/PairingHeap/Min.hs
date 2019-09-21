{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Data.MinHeap where

import           Data.Function
import qualified Data.List     as L
import           Data.Monoid
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
import           Data.Semigroup as Semigroup
#endif
import           GHC.Exts

data MinHeap a = MinFork !a [MinHeap a] | MinEmpty

_Hempty :: MinHeap a
_Hempty = MinEmpty
{-# INLINE _Hempty #-}

_Hsingleton :: a -> MinHeap a
_Hsingleton x = MinFork x []
{-# INLINE _Hsingleton #-}

_Hnull :: MinHeap a -> Bool
_Hnull MinEmpty      = True
_Hnull (MinFork _ _) = False
{-# INLINE _Hnull #-}

_Hinsert :: Ord a => a -> MinHeap a -> MinHeap a
_Hinsert x = _Hmerge (MinFork x [])
{-# INLINE _Hinsert #-}

_HminElem :: MinHeap a -> Maybe a
_HminElem (MinFork x _) = Just x
_HminElem MinEmpty      = Nothing
{-# INLINE _HminElem #-}

_HdeleteMin :: Ord a => MinHeap a -> Maybe (MinHeap a)
_HdeleteMin (MinFork _ hs) = Just $ _HmergePairs hs
_HdeleteMin MinEmpty       = Nothing
{-# INLINE _HdeleteMin #-}

_HdeleteFindMin :: Ord a => MinHeap a -> Maybe (a, MinHeap a)
_HdeleteFindMin (MinFork x hs) = Just (x, _HmergePairs hs)
_HdeleteFindMin MinEmpty       = Nothing
{-# INLINE _HdeleteFindMin #-}

_Hmerge :: Ord a => MinHeap a -> MinHeap a -> MinHeap a
_Hmerge hx@(MinFork x hxs) hy@(MinFork y hys)
  | x <= y    = MinFork x (hy:hxs)
  | otherwise = MinFork y (hx:hys)
_Hmerge MinEmpty hy = hy
_Hmerge hx _ = hx
{-# INLINE _Hmerge #-}

_HmergePairs :: Ord a => [MinHeap a] -> MinHeap a
_HmergePairs (x:y:hs) = (x <> y) <> _HmergePairs hs
_HmergePairs [x]      = x
_HmergePairs []       = MinEmpty
{-# INLINE _HmergePairs #-}

instance Ord a => Eq (MinHeap a) where
    (==) = (==) `on` toList

instance Ord a => Ord (MinHeap a) where
    compare = compare `on` toList

instance Ord a => IsList (MinHeap a) where
    type Item (MinHeap a) = a
    fromList xs = _HmergePairs $ map _Hsingleton xs
    toList = L.unfoldr _HdeleteFindMin

instance (Show a, Ord a) => Show (MinHeap a) where
    show = show . toList


#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
instance Ord a => Semigroup.Semigroup (MinHeap a) where
  (<>) = _Hmerge
#endif

instance Ord a => Monoid (MinHeap a) where
    mempty = _Hempty
    {-# INLINE mempty #-}
    mconcat = _HmergePairs
    {-# INLINE mconcat #-}
#if MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
#elif MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = _Hmerge
    {-# INLINE mappend #-}
#endif
