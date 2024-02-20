module Data.Monoid.Action where

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
