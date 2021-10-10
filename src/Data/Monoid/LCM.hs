{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Monoid.LCM where

import Data.Coerce (coerce)
import qualified Data.Foldable as F

{- |
>>> mempty :: LCM Int
LCM {getLCM = 1}
>>> LCM (-2) <> LCM 3
LCM {getLCM = 6}
>>> LCM (-1) <> mempty
LCM {getLCM = 1}
-}
newtype LCM a = LCM {getLCM :: a}
  deriving (Eq, Show)

instance (Integral a) => Semigroup (LCM a) where
  (<>) = coerce (lcm @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (LCM a) where
  mempty = LCM 1
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}
