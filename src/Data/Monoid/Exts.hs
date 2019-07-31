{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Monoid.Exts where

import           Data.Bits
import           Data.Coerce
import qualified Data.Foldable  as F
import           Data.Monoid
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup as Semigroup
#endif

-- |
-- >>> mempty :: GCD Int
-- GCD {getGCD = 0}
-- >>> GCD (-4) <> GCD 6
-- GCD {getGCD = 2}
-- >>> GCD (-1) <> mempty
-- GCD {getGCD = 1}
newtype GCD a = GCD { getGCD :: a } deriving (Eq, Show, Num)

#if MIN_VERSION_base(4,9,0)
instance (Integral a) => Semigroup.Semigroup (GCD a) where
  (<>) = coerce (gcd :: a -> a -> a)
  {-# INLINE (<>) #-}
#endif

instance (Num a, Integral a) => Monoid (GCD a) where
    mempty = 0
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = coerce (gcd :: a -> a -> a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: LCM Int
-- LCM {getLCM = 0}
-- >>> LCM (-2) <> LCM 3
-- LCM {getLCM = 6}
-- >>> LCM (-1) <> mempty
-- LCM {getLCM = 1}
newtype LCM a = LCM { getLCM :: a } deriving (Eq, Show, Num)

#if MIN_VERSION_base(4,9,0)
instance (Integral a) => Semigroup.Semigroup (LCM a) where
  (<>) = coerce (lcm :: a -> a -> a)
  {-# INLINE (<>) #-}
#endif

instance (Num a, Integral a) => Monoid (LCM a) where
    mempty = 1
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = coerce (lcm :: a -> a -> a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: BitAnd Int
-- BitAnd {getBitAnd = -1}
-- >>> mempty :: BitAnd Word
-- BitAnd {getBitAnd = 18446744073709551615}
newtype BitAnd a = BitAnd { getBitAnd :: a } deriving (Eq, Show)

#if MIN_VERSION_base(4,9,0)
instance (Bits a) => Semigroup.Semigroup (BitAnd a) where
  (<>) = coerce ((.&.) :: a -> a -> a)
  {-# INLINE (<>) #-}
#endif

instance (Bits a) => Monoid (BitAnd a) where
    mempty = coerce $ complement (zeroBits :: a)
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = coerce ((.&.) :: a -> a -> a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: BitOr Int
-- BitOr {getBitOr = 0}
newtype BitOr a = BitOr { getBitOr :: a } deriving (Eq, Show)

#if MIN_VERSION_base(4,9,0)
instance (Bits a) => Semigroup.Semigroup (BitOr a) where
  (<>) = coerce ((.|.) :: a -> a -> a)
  {-# INLINE (<>) #-}
#endif

instance (Bits a) => Monoid (BitOr a) where
    mempty = coerce (zeroBits :: a)
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = coerce ((.|.) :: a -> a -> a)
    {-# INLINE mappend #-}
#endif

-- |
-- >>> mempty :: BitXor Int
-- BitXor {getBitXor = 0}
newtype BitXor a = BitXor { getBitXor :: a } deriving (Eq, Show)

#if MIN_VERSION_base(4,9,0)
instance (Bits a) => Semigroup.Semigroup (BitXor a) where
  (<>) = coerce (xor :: a -> a -> a)
  {-# INLINE (<>) #-}
#endif

instance (Bits a) => Monoid (BitXor a) where
    mempty = coerce (zeroBits :: a)
    {-# INLINE mempty #-}
    mconcat = F.foldl' mappend mempty
    {-# INLINE mconcat #-}
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = coerce (xor :: a -> a -> a)
    {-# INLINE mappend #-}
#endif