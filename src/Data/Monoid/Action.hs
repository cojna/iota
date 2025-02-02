module Data.Monoid.Action where

import Data.Coerce
import Data.Semigroup

{- | * @mact mempty = id@
   * @mact (f <> g) = mact f . mact g@
-}
class (Monoid f) => MonoidAction f a where
  mact :: f -> a -> a

instance MonoidAction () m where
  mact = const id
  {-# INLINE mact #-}

instance (Monoid m) => MonoidAction m m where
  mact = (<>)
  {-# INLINE mact #-}

instance (Num a) => MonoidAction (Sum a) a where
  mact = coerce ((+) @a)
  {-# INLINE mact #-}

instance (Num a) => MonoidAction (Product a) a where
  mact = coerce ((*) @a)
  {-# INLINE mact #-}

instance (Num a) => MonoidAction (Product a) (Sum a) where
  mact = coerce ((*) @a)
  {-# INLINE mact #-}

instance (Ord a, Bounded a) => MonoidAction (Max a) a where
  mact = coerce (max @a)
  {-# INLINE mact #-}

instance (Ord a, Bounded a) => MonoidAction (Min a) a where
  mact = coerce (min @a)
  {-# INLINE mact #-}
