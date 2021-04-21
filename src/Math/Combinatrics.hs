{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.Combinatrics where

import Data.Coerce
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.TypeLits

import Data.GaloisField (GF (GF), natValAsInt)
import My.Prelude (rep, rep1)

-- | /O(1)/
fact :: (KnownNat p) => Int -> GF p
fact = U.unsafeIndex factCache
{-# INLINE fact #-}

-- | /O(1)/
recipFact :: (KnownNat p) => Int -> GF p
recipFact = U.unsafeIndex recipFactCache
{-# INLINE recipFact #-}

{- | /O(1)/

 n < p
-}
perm :: (KnownNat p) => Int -> Int -> GF p
perm n k
  | 0 <= k, k <= n = fact n * recipFact (n - k)
  | otherwise = GF 0
{-# INLINE perm #-}

{- | /O(1)/

 n < p
-}
comb :: (KnownNat p) => Int -> Int -> GF p
comb n k
  | 0 <= k, k <= n = fact n * recipFact (n - k) * recipFact k
  | otherwise = GF 0
{-# INLINE comb #-}

defaultFactCacheSize :: Int
defaultFactCacheSize = 1024 * 1024

factCache :: forall p. (KnownNat p) => U.Vector (GF p)
factCache = U.scanl' (\x y -> x * coerce y) (GF 1) $ U.generate size (+ 1)
  where
    size = min defaultFactCacheSize (natValAsInt (Proxy @p) - 1)
{-# NOINLINE factCache #-}

recipFactCache :: forall p. (KnownNat p) => U.Vector (GF p)
recipFactCache =
  U.scanr' ((*) . coerce) (1 / factCache U.! size) $
    U.generate size (+ 1)
  where
    size = min defaultFactCacheSize (natValAsInt (Proxy @p) - 1)
{-# NOINLINE recipFactCache #-}

{- | Lucas's theorem

/O(log N)/
-}
combSmall :: forall p. (KnownNat p) => Int -> Int -> GF p
combSmall = go (GF 1)
  where
    p = natValAsInt (Proxy @p)
    go !acc 0 0 = acc
    go !acc !n !r = go (acc * c) qn qr
      where
        (qn, rn) = quotRem n p
        (qr, rr) = quotRem r p
        c = U.unsafeIndex combSmallTable (rn * p + rr)

-- | /O(p ^ 2)/
combSmallTable :: forall p. (KnownNat p) => U.Vector (GF p)
combSmallTable = U.create $ do
  dp <- UM.replicate (n * n) (GF 0)
  rep n $ \i -> do
    UM.unsafeWrite dp (ix i 0) (GF 1)
    UM.unsafeWrite dp (ix i i) (GF 1)
  rep1 (n - 1) $ \x -> do
    rep1 (x - 1) $ \y -> do
      (+) <$> UM.unsafeRead dp (ix (x - 1) (y - 1))
        <*> UM.unsafeRead dp (ix (x - 1) y)
        >>= UM.unsafeWrite dp (ix x y)
  return dp
  where
    n = natValAsInt (Proxy @p)
    ix x y = x * n + y
{-# NOINLINE combSmallTable #-}
