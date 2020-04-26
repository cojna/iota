{-# LANGUAGE TypeFamilies #-}

module Data.Queue where

import           Data.Function
import           GHC.Exts

data Queue a = Q [a] [a]

emptyQ :: Queue a
emptyQ = Q [] []
{-# INLINE emptyQ #-}

nullQ :: Queue a -> Bool
nullQ (Q fs rs) = null fs && null rs
{-# INLINE nullQ #-}

singletonQ :: a -> Queue a
singletonQ x = Q [x] []
{-# INLINE singletonQ #-}

headQ :: Queue a -> Maybe (a, Queue a)
headQ (Q (f:fs) rs) = Just (f, Q fs rs)
headQ (Q [] rs) = case reverse rs of
    (r:rs') -> Just (r, Q rs' [])
    []      -> Nothing
{-# INLINE headQ #-}

insertQ :: a -> Queue a -> Queue a
insertQ = flip snocQ
{-# INLINE insertQ #-}

snocQ :: Queue a -> a -> Queue a
snocQ (Q fs rs) x = Q fs (x:rs)
{-# INLINE snocQ #-}

consQ :: a -> Queue a -> Queue a
consQ x (Q fs rs) = Q (x:fs) rs
{-# INLINE consQ #-}

(|>) :: Queue a -> a -> Queue a
(|>) = snocQ
{-# INLINE (|>) #-}

(<|) :: a -> Queue a -> Queue a
(<|) = consQ
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

