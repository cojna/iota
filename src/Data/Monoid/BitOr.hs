{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Monoid.BitOr where

import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F

{- |
>>> mempty :: BitOr Int
BitOr {getBitOr = 0}
-}
newtype BitOr a = BitOr {getBitOr :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitOr a) where
  (<>) = coerce ((.|.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitOr a) where
  mempty = coerce (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}
