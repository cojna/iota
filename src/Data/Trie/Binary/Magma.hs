{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Trie.Binary.Magma where

import Data.Bits
import Data.Coerce
import GHC.Exts
import GHC.TypeLits

{- ORMOLU_DISABLE -}
-- * Examples
{- $examples
=== __ABC308G Minimum Xor Pair Query__

> data A = Done !Word | Partial !Word
>
> -- non-associative
> instance Magma A where
>     magma = m
>       where
>         m (Done x) (Done y) = Done (min x y)
>         m (Done x) Partial{} = Done x
>         m Partial{} (Done y) = Done y
>         m (Partial x) (Partial y) = Done (xor x y)
>     {-# INLINE magma #-}
>
> type B = Int
>
> tip :: Word -> B -> A
> tip w b
>     | b >= 2 = Done 0
>     | otherwise = Partial w
> {-# INLINE tip #-}
>
> ins :: Maybe B -> Maybe B
> ins = Just . maybe 1 (+ 1)
> {-# INLINE ins #-}
>
> del :: Maybe B -> Maybe B
> del ma = case ma of
>     Just a | a > 1 -> Just (a - 1)
>     _ -> Nothing
> {-# INLINE del #-}
>
> insert :: (KnownNat h) => Word -> Trie h A B -> Trie h A B
> insert = alterWith tip ins
> {-# INLINE insert #-}
>
> delete :: (KnownNat h) => Word -> Trie h A B -> Trie h A B
> delete = alterWith tip del
> {-# INLINE delete #-}
>
> magmaAll :: Trie h A B -> Maybe A
> magmaAll = magmaAllWith tip
> {-# INLINE magmaAll #-}
-}
{- ORMOLU_ENABLE -}

-- * Binary Trie

data Trie (h :: Nat) a b
  = Bin !a !(Trie h a b) !(Trie h a b)
  | Tip !Word !b
  | Nil
  deriving (Show)

magmaAllWith ::
  -- | Tip to Magma
  (Word -> b -> a) ->
  Trie h a b ->
  Maybe a
magmaAllWith _ (Bin a _ _) = Just a
magmaAllWith u (Tip w b) = Just (u w b)
magmaAllWith _ Nil = Nothing

binWith ::
  (Magma a) =>
  -- | Tip to Magma
  (Word -> b -> a) ->
  -- | left child
  Trie h a b ->
  -- | right child
  Trie h a b ->
  Trie h a b
binWith _ t0@(Bin a0 _ _) t1@(Bin a1 _ _) = Bin (magma a0 a1) t0 t1
binWith u t0@(Bin a0 _ _) t1@(Tip w1 b1) = Bin (magma a0 (u w1 b1)) t0 t1
binWith _ t0@(Bin a0 _ _) t1@Nil = Bin a0 t0 t1
binWith u t0@(Tip w0 b0) t1@(Bin a1 _ _) = Bin (magma (u w0 b0) a1) t0 t1
binWith u t0@(Tip w0 b0) t1@(Tip w1 b1) = Bin (magma (u w0 b0) (u w1 b1)) t0 t1
binWith _ t0@Tip{} Nil = t0
binWith _ t0@Nil t1@(Bin a1 _ _) = Bin a1 t0 t1
binWith _ Nil t1@Tip{} = t1
binWith _ Nil Nil = Nil

alterWith ::
  forall h a b.
  (Magma a, KnownNat h) =>
  -- | Tip to Magma
  (Word -> b -> a) ->
  -- | insert/delete
  (Maybe b -> Maybe b) ->
  Word ->
  Trie h a b ->
  Trie h a b
alterWith u f v = go (unsafeShiftL 1 (fromIntegral (natVal' @h proxy#) - 1))
  where
    go !flg trie = case trie of
      Bin _ t0 t1
        | v .&. flg == 0 -> binWith u (go (unsafeShiftR flg 1) t0) t1
        | otherwise -> binWith u t0 (go (unsafeShiftR flg 1) t1)
      Tip w a
        | v == w -> maybe Nil (Tip v) $ f (Just a)
        | w .&. flg == 0 -> go flg $ Bin (u w a) (Tip w a) Nil
        | otherwise -> go flg $ Bin (u w a) Nil (Tip w a)
      Nil -> maybe Nil (Tip v) $ f Nothing
{-# INLINE alterWith #-}

-- * Magma
class Magma a where
  magma :: a -> a -> a

newtype WrappedSemigroup m = WrapSemigroup m

instance (Semigroup m) => Magma (WrappedSemigroup m) where
  magma = coerce ((<>) :: m -> m -> m)
  {-# INLINE magma #-}
