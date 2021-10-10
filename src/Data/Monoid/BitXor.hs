{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Monoid.BitXor where

import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F

{- |
>>> mempty :: BitXor Int
BitXor {getBitXor = 0}
-}
newtype BitXor a = BitXor {getBitXor :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitXor a) where
  (<>) = coerce (xor @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitXor a) where
  mempty = coerce (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}
