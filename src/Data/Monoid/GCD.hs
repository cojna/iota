{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Monoid.GCD where

import Data.Coerce (coerce)
import qualified Data.Foldable as F

{- |
>>> mempty :: GCD Int
GCD {getGCD = 0}
>>> GCD (-4) <> GCD 6
GCD {getGCD = 2}
>>> GCD (-1) <> mempty
GCD {getGCD = 1}
-}
newtype GCD a = GCD {getGCD :: a}
  deriving (Eq, Show)

instance (Integral a) => Semigroup (GCD a) where
  (<>) = coerce (gcd @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (GCD a) where
  mempty = GCD 0
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}