{-# LANGUAGE BangPatterns, MagicHash, RecordWildCards, UnboxedTuples #-}

module Math.NTT where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM
import           GHC.Exts

import           Math.Modulus                      (powMod, recipMod)
import           Utils                             (rep, stream, streamR,
                                                    unsafeShiftRL)

-- | Number Theoretic Transform
--
-- /O(n log n)/
--
-- >>> ntt 998244353 3 [1,1,1,1]
-- [4,0,0,0]
-- >>> ntt 469762049 3 [123,0,0,0]
-- [123,123,123,123]
ntt
    :: Int          -- ^ prime (c * 2 ^ k + 1)
    -> Int          -- ^ primitive root
    -> U.Vector Int -- ^ n = 2 ^ i, n < 2 ^ k
    -> U.Vector Int
ntt p g f = U.modify (butterfly nr) f
  where
    nr = buildNTTRunner p g

{-# INLINE ntt #-}

intt :: Int -> Int -> U.Vector Int -> U.Vector Int
intt p g f = U.map (mulNR nr invn) $ U.modify (invButterfly nr) f
  where
    nr = buildNTTRunner p g
    !invn = recipMod (U.length f) p
{-# INLINE intt #-}

-- |
-- >>> convolute 998244353 3 [1,1,1,0] [1,1,1,0]
-- [1,2,3,2,1,0,0]
-- >>> convolute 998244353 3 [1,1,1] [1,1,1,0]
-- [1,2,3,2,1,0]
convolute :: Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
convolute p g xs ys = U.create $ do
    mxs <- UM.replicate len 0
    U.unsafeCopy (UM.take n mxs) xs
    butterfly nr mxs
    mys <- UM.replicate len 0
    U.unsafeCopy (UM.take m mys) ys
    butterfly nr mys
    rep len $ \i -> do
        yi <- UM.unsafeRead mys i
        UM.unsafeModify mxs (mulNR nr yi) i
    invButterfly nr mxs
    rep (n + m - 1) $ \i -> do
        UM.unsafeModify mxs (mulNR nr ilen) i
    return $ UM.take (n + m - 1) mxs
  where
    !nr = buildNTTRunner p g
    n = U.length xs
    m = U.length ys
    !h = head [i | i<-[0..], n + m - 1 <= unsafeShiftL 1 i]
    !len = unsafeShiftL 1 h
    !ilen = recipMod len p
{-# INLINE convolute #-}

data NTTRunner = NTTRunner
    { pNR    :: !Int
    , gNR    :: !Int
    , ipNR   :: !Word
    , sesNR  :: !(U.Vector Int)
    , siesNR :: !(U.Vector Int)
    }

buildNTTRunner :: Int -> Int -> NTTRunner
buildNTTRunner pNR gNR = NTTRunner{..}
  where
    ipNR = quot (complement 0) (fromIntegral pNR) + 1

    ctz = countTrailingZeros (pNR - 1)
    !e = powMod gNR (unsafeShiftR (pNR - 1) ctz) pNR
    !ie = recipMod e pNR

    es  = U.reverse $ U.iterateN (ctz - 1) (\x -> x *% x) e
    ies = U.reverse $ U.iterateN (ctz - 1) (\x -> x *% x) ie

    sesNR = U.zipWith (*%) es $ U.scanl' (*%) 1 ies
    siesNR = U.zipWith (*%) ies $ U.scanl' (*%) 1 es
    x *% y = x * y `rem` pNR

addNR :: NTTRunner -> Int -> Int -> Int
addNR (NTTRunner (I# m#) _ _ _ _) (I# x#) (I# y#)
    = case x# +# y# of
        z# -> I# (z# -# ((z# >=# m#) *# m#))
{-# INLINE addNR #-}

subNR :: NTTRunner -> Int -> Int -> Int
subNR (NTTRunner (I# m#) _ _ _ _) (I# x#) (I# y#)
    = case x# -# y# of
        z# -> I# (z# +# ((z# <# 0#) *# m#))
{-# INLINE subNR #-}

mulNR :: NTTRunner -> Int -> Int -> Int
mulNR (NTTRunner (I# m0#) _ (W# im#) _ _) (I# x#) (I# y#)
    = case timesWord# (int2Word# x#) (int2Word# y#) of
        z# -> case timesWord2# z# im# of
            (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
                v# | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
                   | otherwise -> I# (word2Int# v#)
  where
    m# = int2Word# m0#
{-# INLINE mulNR #-}

butterfly :: (PrimMonad m) => NTTRunner -> UM.MVector (PrimState m) Int -> m ()
butterfly nr@NTTRunner{..} mvec = do
    flip MS.mapM_ (stream 1 (h + 1)) $ \ph -> do
        let !w = unsafeShiftL 1 (ph - 1)
            !p = unsafeShiftL 1 (h - ph)
        void $ MS.foldlM'
            (\acc s -> do
                let offset = unsafeShiftL s (h - ph + 1)
                flip MS.mapM_ (stream offset (offset + p)) $ \i -> do
                    l <- UM.unsafeRead mvec i
                    r <- mulNR nr acc <$!> UM.unsafeRead mvec (i + p)
                    UM.unsafeWrite mvec (i + p) $ subNR nr l r
                    UM.unsafeWrite mvec i $ addNR nr l r
                return $! mulNR nr acc $ U.unsafeIndex siesNR (countTrailingZeros (complement s))
            ) 1 (stream 0 w)
  where
    n = UM.length mvec
    !h = head [i | i<-[0..], n <= unsafeShiftL 1 i]
{-# INLINE butterfly #-}

invButterfly :: (PrimMonad m) => NTTRunner -> UM.MVector (PrimState m) Int -> m ()
invButterfly nr@NTTRunner{..} mvec = void $ do
    flip MS.mapM_ (streamR 1 (h + 1)) $ \ph -> do
        let !w = unsafeShiftL 1 (ph - 1)
            !p = unsafeShiftL 1 (h - ph)
        MS.foldlM'
            (\acc s -> do
                let offset = unsafeShiftL s (h - ph + 1)
                flip MS.mapM_ (stream offset (offset + p)) $ \i -> do
                    l <- UM.unsafeRead mvec i
                    r <- UM.unsafeRead mvec (i + p)
                    UM.unsafeWrite mvec (i + p) $ mulNR nr acc (pNR + l - r)
                    UM.unsafeWrite mvec i $ addNR nr l r
                return $! mulNR nr acc $ U.unsafeIndex sesNR (countTrailingZeros (complement s))
            ) 1 (stream 0 w)
  where
    n = UM.length mvec
    !h = head [i | i<-[0..], n <= unsafeShiftL 1 i]
{-# INLINE invButterfly #-}

-- |
-- >>> growToPowerOfTwo [1,2,3]
-- [1,2,3,0]
growToPowerOfTwo :: U.Vector Int -> U.Vector Int
growToPowerOfTwo v
    | U.null v = U.singleton 0
    | U.length v == 1 = v
    | n <- unsafeShiftRL (-1) (countLeadingZeros (U.length v - 1)) + 1
        = v U.++ U.replicate (n - U.length v) 0

-- |
-- >>> extendToPowerOfTwo 0
-- 1
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1
