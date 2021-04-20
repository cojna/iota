{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.Combinatrics where

import Data.Coerce
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import GHC.TypeLits

import Data.GaloisField

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
