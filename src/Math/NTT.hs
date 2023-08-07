{-# LANGUAGE RecordWildCards #-}

module Math.NTT where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import qualified Data.List as L
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.TypeLits (KnownNat)

import Data.GaloisField (GF (GF), natValAsInt, reifyNat)
import Math.Prime (primeFactors)
import My.Prelude (
  rep,
  unsafeShiftRL,
  (..<),
  (>..),
 )

{- | Number Theoretic Transform
p: prime (c * 2 ^ k + 1)

n = 2 ^ i, n < 2 ^ k

 /O(n log n)/

>>> ntt @998244353 [1,1,1,1]
[4,0,0,0]
>>> ntt @469762049 [123,0,0,0]
[123,123,123,123]
-}
ntt ::
  forall p.
  (KnownNat p) =>
  U.Vector (GF p) ->
  U.Vector (GF p)
ntt = U.modify butterfly
{-# INLINE ntt #-}

intt :: forall p. (KnownNat p) => U.Vector (GF p) -> U.Vector (GF p)
intt f = U.map (* invn) $ U.modify invButterfly f
  where
    !invn = recip (GF $ U.length f)
{-# INLINE intt #-}

{- |
>>> convolute @998244353 [1,1,1,0] [1,1,1,0]
[1,2,3,2,1,0,0]
>>> convolute @998244353 [1,1,1] [1,1,1,0]
[1,2,3,2,1,0]
-}
convolute ::
  forall p.
  (KnownNat p) =>
  U.Vector (GF p) ->
  U.Vector (GF p) ->
  U.Vector (GF p)
convolute xs ys = U.create $ do
  mxs <- UM.replicate len (GF 0)
  U.unsafeCopy (UM.take n mxs) xs
  butterfly mxs
  mys <- UM.replicate len (GF 0)
  U.unsafeCopy (UM.take m mys) ys
  butterfly mys
  rep len $ \i -> do
    yi <- UM.unsafeRead mys i
    UM.unsafeModify mxs (* yi) i
  invButterfly mxs
  rep (n + m - 1) $ \i -> do
    UM.unsafeModify mxs (* ilen) i
  return $ UM.take (n + m - 1) mxs
  where
    n = U.length xs
    m = U.length ys
    !h = head [i | i <- [0 ..], n + m - 1 <= unsafeShiftL 1 i]
    !len = unsafeShiftL 1 h
    !ilen = recip (GF len)
{-# INLINE convolute #-}

data NTTRunner p = NTTRunner
  { sesNR :: !(U.Vector (GF p))
  , siesNR :: !(U.Vector (GF p))
  }

nttRunner :: forall p. (KnownNat p) => NTTRunner p
nttRunner = NTTRunner{..}
  where
    p = natValAsInt (Proxy @p)
    g = primitiveRoot p

    ctz = countTrailingZeros (p - 1)
    !e = GF g ^ unsafeShiftR (p - 1) ctz
    !ie = recip e

    es = U.reverse $ U.iterateN (ctz - 1) (\x -> x * x) e
    ies = U.reverse $ U.iterateN (ctz - 1) (\x -> x * x) ie

    sesNR = U.zipWith (*) es $ U.scanl' (*) 1 ies
    siesNR = U.zipWith (*) ies $ U.scanl' (*) 1 es
{-# NOINLINE nttRunner #-}

butterfly ::
  (KnownNat p, PrimMonad m) =>
  UM.MVector (PrimState m) (GF p) ->
  m ()
butterfly mvec = do
  flip MS.mapM_ (1 ..< (h + 1)) $ \ph -> do
    let !w = unsafeShiftL 1 (ph - 1)
        !p = unsafeShiftL 1 (h - ph)
    void $
      MS.foldlM'
        ( \acc s -> do
            let offset = unsafeShiftL s (h - ph + 1)
            flip MS.mapM_ (offset ..< (offset + p)) $ \i -> do
              l <- UM.unsafeRead mvec i
              r <- (* acc) <$!> UM.unsafeRead mvec (i + p)
              UM.unsafeWrite mvec (i + p) $ l - r
              UM.unsafeWrite mvec i $ l + r
            return $! acc * U.unsafeIndex siesNR (countTrailingZeros (complement s))
        )
        1
        (0 ..< w)
  where
    n = UM.length mvec
    !h = head [i | i <- [0 ..], n <= unsafeShiftL 1 i]
    NTTRunner{..} = nttRunner
{-# INLINE butterfly #-}

invButterfly ::
  forall p m.
  (KnownNat p, PrimMonad m) =>
  UM.MVector (PrimState m) (GF p) ->
  m ()
invButterfly mvec = void $ do
  flip MS.mapM_ ((h + 1) >.. 1) $ \ph -> do
    let !w = unsafeShiftL 1 (ph - 1)
        !p = unsafeShiftL 1 (h - ph)
    MS.foldlM'
      ( \acc s -> do
          let offset = unsafeShiftL s (h - ph + 1)
          flip MS.mapM_ (offset ..< (offset + p)) $ \i -> do
            l <- UM.unsafeRead mvec i
            r <- UM.unsafeRead mvec (i + p)
            UM.unsafeWrite mvec (i + p) $ acc * (l - r)
            UM.unsafeWrite mvec i $ l + r
          return $! acc * U.unsafeIndex sesNR (countTrailingZeros (complement s))
      )
      1
      (0 ..< w)
  where
    n = UM.length mvec
    !h = head [i | i <- [0 ..], n <= unsafeShiftL 1 i]
    NTTRunner{..} = nttRunner
{-# INLINE invButterfly #-}

{- |
>>> growToPowerOfTwo (U.fromListN 3 [1::Int,2,3])
[1,2,3,0]
-}
growToPowerOfTwo :: (Num a, U.Unbox a) => U.Vector a -> U.Vector a
growToPowerOfTwo v
  | U.null v = U.singleton 0
  | U.length v == 1 = v
  | n <- unsafeShiftRL (-1) (countLeadingZeros (U.length v - 1)) + 1 =
      v U.++ U.replicate (n - U.length v) 0

{- |
>>> extendToPowerOfTwo 0
1
-}
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
  | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
  | otherwise = 1

{- |
>>> primitiveRoot 2
1
>>> primitiveRoot 998244353
3
-}
primitiveRoot ::
  -- | prime
  Int ->
  Int
primitiveRoot 2 = 1
primitiveRoot prime = reifyNat prime $ \proxy ->
  head [g | g <- [2 ..], all (check (toGF proxy g)) ps]
  where
    !ps = map head . L.group $ primeFactors (prime - 1)

    toGF :: Proxy p -> Int -> GF p
    toGF _ = GF

    check :: (KnownNat p) => GF p -> Int -> Bool
    check g p = g ^ quot (prime - 1) p /= GF 1
