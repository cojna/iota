{-# LANGUAGE TypeFamilies #-}

module Data.Queue where

import           Data.Function
import           GHC.Exts

data Queue a = Q [a] [a]

_Qempty :: Queue a
_Qempty = Q [] []
{-# INLINE _Qempty #-}

_Qnull :: Queue a -> Bool
_Qnull (Q fs rs) = null fs && null rs
{-# INLINE _Qnull #-}

_Qsingleton :: a -> Queue a
_Qsingleton x = Q [x] []
{-# INLINE _Qsingleton #-}

_Qhead :: Queue a -> Maybe (a, Queue a)
_Qhead (Q (f:fs) rs) = Just (f, Q fs rs)
_Qhead (Q [] rs) = case reverse rs of
    (r:rs') -> Just (r, Q rs' [])
    []      -> Nothing
{-# INLINE _Qhead #-}

_Qinsert :: a -> Queue a -> Queue a
_Qinsert = flip _Qsnoc
{-# INLINE _Qinsert #-}

_Qsnoc :: Queue a -> a -> Queue a
_Qsnoc (Q fs rs) x = Q fs (x:rs)
{-# INLINE _Qsnoc #-}

_Qcons :: a -> Queue a -> Queue a
_Qcons x (Q fs rs) = Q (x:fs) rs
{-# INLINE _Qcons #-}

(|>) :: Queue a -> a -> Queue a
(|>) = _Qsnoc
{-# INLINE (|>) #-}

(<|) :: a -> Queue a -> Queue a
(<|) = _Qcons
{-# INLINE (<|) #-}

instance IsList (Queue a) where
    type Item (Queue a) = a
    fromList xs = Q xs []
    toList (Q fs rs) = fs ++ reverse rs

instance (Eq a) => Eq (Queue a) where
    (==) = (==) `on` toList

instance (Ord a) => Ord (Queue a) where
    compare = compare `on` toList

instance (Show a) => Show (Queue a) where
    show = show . toList

instance Functor Queue where
    fmap f (Q fs rs) = Q (fmap f fs) (fmap f rs)

