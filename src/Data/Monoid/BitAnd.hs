{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Monoid.BitAnd where

import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F

{- |
>>> mempty :: BitAnd Int
BitAnd {getBitAnd = -1}
>>> mempty :: BitAnd Word
BitAnd {getBitAnd = 18446744073709551615}
-}
newtype BitAnd a = BitAnd {getBitAnd :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitAnd a) where
  (<>) = coerce ((.&.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitAnd a) where
  mempty = coerce $ complement (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}
