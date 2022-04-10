{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Math.Combinatrics where

import Data.Coerce
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts
import GHC.TypeLits

import Data.GaloisField (GF (GF), natValAsInt)
import My.Prelude (rep, rep1)

newtype FactCache p = FactCache (U.Vector (GF p))
type HasFactCache (p :: Nat) = (?factCache :: FactCache p)

newtype RecipFactCache p = RecipFactCache (U.Vector (GF p))
type HasRecipFactCache (p :: Nat) = (?recipFactCache :: RecipFactCache p)

type HasCombCache (p :: Nat) = (HasFactCache p, HasRecipFactCache p)

{- | /O(1)/

>>> :set -XTypeApplications -XDataKinds
>>> withFactCache @1000000007 10 $ fact 10
3628800
-}
fact :: (HasFactCache p, KnownNat p) => Int -> GF p
fact = U.unsafeIndex (coerce ?factCache)
{-# INLINE fact #-}

-- | /O(1)/
recipFact :: (HasRecipFactCache p, KnownNat p) => Int -> GF p
recipFact = U.unsafeIndex (coerce ?recipFactCache)
{-# INLINE recipFact #-}

{- | /O(1)/

 n < p
-}
perm :: (HasFactCache p, HasRecipFactCache p, KnownNat p) => Int -> Int -> GF p
perm n k
  | 0 <= k, k <= n = fact n * recipFact (n - k)
  | otherwise = GF 0
{-# INLINE perm #-}

{- | /O(1)/

 n < p
-}
comb :: (HasFactCache p, HasRecipFactCache p, KnownNat p) => Int -> Int -> GF p
comb n k
  | 0 <= k, k <= n = fact n * recipFact (n - k) * recipFact k
  | otherwise = GF 0
{-# INLINE comb #-}

{- | /O(r)/

>>> combNaive 64 32
1832624140942590534
>>> combNaive 123456789 2
7620789313366866
>>> combNaive 123 456
0
-}
combNaive :: Int -> Int -> Int
combNaive n@(I# ni#) r@(I# ri#)
  | 0 <= r, r <= n = go# 1## 1##
  | otherwise = 0
  where
    n# = int2Word# ni#
    r# = int2Word# ri#
    go# acc# i#
      | isTrue# (leWord# i# r#) =
        case timesWord2# acc# (minusWord# n# (minusWord# i# 1##)) of
          (# x#, y# #) -> case quotRemWord2# x# y# i# of
            (# z#, _ #) -> go# z# (plusWord# i# 1##)
      | otherwise = I# (word2Int# acc#)

buildFactCache :: forall p. (KnownNat p) => Int -> FactCache p
buildFactCache n =
  FactCache
    . U.scanl' (\x y -> x * coerce y) (GF 1)
    $ U.generate size (+ 1)
  where
    size = min n (natValAsInt (Proxy @p) - 1)

withFactCache :: forall p r. (KnownNat p) => Int -> (HasFactCache p => r) -> r
withFactCache n x = let ?factCache = cache in x
  where
    !cache = buildFactCache n
{-# INLINE withFactCache #-}

buildRecipFactCache :: forall p. (HasFactCache p, KnownNat p) => Int -> RecipFactCache p
buildRecipFactCache n =
  RecipFactCache
    . U.scanr' ((*) . coerce) (1 / fact size)
    $ U.generate size (+ 1)
  where
    size = min n (natValAsInt (Proxy @p) - 1)

withRecipFactCache :: forall p r. (HasFactCache p, KnownNat p) => Int -> (HasRecipFactCache p => r) -> r
withRecipFactCache n x = let ?recipFactCache = cache in x
  where
    !cache = buildRecipFactCache n
{-# INLINE withRecipFactCache #-}

withCombCache :: forall p r. (KnownNat p) => Int -> (HasCombCache p => r) -> r
withCombCache n x = withFactCache n $ withRecipFactCache n x
{-# INLINE withCombCache #-}

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
