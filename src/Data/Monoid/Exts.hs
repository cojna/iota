{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications                                     #-}

module Data.Monoid.Exts where

import           Data.Bits
import           Data.Coerce
import qualified Data.Foldable as F

-- |
-- >>> mempty :: GCD Int
-- GCD {getGCD = 0}
-- >>> GCD (-4) <> GCD 6
-- GCD {getGCD = 2}
-- >>> GCD (-1) <> mempty
-- GCD {getGCD = 1}
newtype GCD a = GCD { getGCD :: a } deriving (Eq, Show, Num)

instance (Integral a) => Semigroup (GCD a) where
  (<>) = coerce (gcd @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (GCD a) where
    mempty = 0
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if !MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
    mappend = coerce (gcd @a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: LCM Int
-- LCM {getLCM = 1}
-- >>> LCM (-2) <> LCM 3
-- LCM {getLCM = 6}
-- >>> LCM (-1) <> mempty
-- LCM {getLCM = 1}
newtype LCM a = LCM { getLCM :: a } deriving (Eq, Show, Num)

instance (Integral a) => Semigroup (LCM a) where
  (<>) = coerce (lcm @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (LCM a) where
    mempty = 1
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if !MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
    mappend = coerce (lcm @a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: BitAnd Int
-- BitAnd {getBitAnd = -1}
-- >>> mempty :: BitAnd Word
-- BitAnd {getBitAnd = 18446744073709551615}
newtype BitAnd a = BitAnd { getBitAnd :: a } deriving (Eq, Show)

instance (Bits a) => Semigroup (BitAnd a) where
  (<>) = coerce ((.&.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitAnd a) where
    mempty = coerce $ complement (zeroBits @a)
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if !MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
    mappend = coerce ((.&.) @a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: BitOr Int
-- BitOr {getBitOr = 0}
newtype BitOr a = BitOr { getBitOr :: a } deriving (Eq, Show)

instance (Bits a) => Semigroup (BitOr a) where
  (<>) = coerce ((.|.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitOr a) where
    mempty = coerce (zeroBits @a)
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if !MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
    mappend = coerce ((.|.) @a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: BitXor Int
-- BitXor {getBitXor = 0}
newtype BitXor a = BitXor { getBitXor :: a } deriving (Eq, Show)

instance (Bits a) => Semigroup (BitXor a) where
  (<>) = coerce (xor @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitXor a) where
    mempty = coerce (zeroBits @a)
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if !MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
    mappend = coerce (xor @a)
    {-# INLINE mappend #-}
#endif
