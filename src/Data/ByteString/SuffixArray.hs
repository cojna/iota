module Data.ByteString.SuffixArray where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Coerce
import Data.Function
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import My.Prelude

newtype SuffixArray a = SuffixArray {getSuffixArray :: U.Vector a}
  deriving (Eq)

instance (Show a, U.Unbox a) => Show (SuffixArray a) where
  show = show . getSuffixArray

indexSA :: (U.Unbox a) => SuffixArray a -> Int -> a
indexSA = coerce U.unsafeIndex
{-# INLINE indexSA #-}

{- | SA-IS /O(n)/

>>> :set -XOverloadedStrings
>>> buildSuffixArray "aaa"
[3,2,1,0]
>>> buildSuffixArray "mississippi"
[11,10,7,4,1,0,9,8,6,3,5,2]
>>> buildSuffixArray "abracadabra"
[11,10,7,0,3,5,8,1,4,6,9,2]
>>> buildSuffixArray "ababab"
[6,4,2,0,5,3,1]
>>> buildSuffixArray ""
[0]
-}
buildSuffixArray :: B.ByteString -> SuffixArray Int32
buildSuffixArray bs = SuffixArray $ U.create $ do
  sa <- UM.replicate (n + 1) (-1)
  if n > 0
    then
      sais sa (maxBound :: Int8) $
        U.scanr' setLS sentinelLS (U.generate n (fromIntegral . B.unsafeIndex bs))
    else UM.write sa 0 0
  return sa
  where
    n = B.length bs

class (Ord a, Num a, Integral a) => LSInt a where
  isL :: a -> Bool
  isS :: a -> Bool
  unLS :: a -> a
  setLS :: a -> a -> a
  sentinelLS :: a
  sentinelLS = 0

instance LSInt Int where
  isL = (< 0)
  {-# INLINE isL #-}
  isS = (>= 0)
  {-# INLINE isS #-}
  unLS = (.&. maxBound)
  {-# INLINE unLS #-}
  setLS c c' = case compare c (unLS c') of
    LT -> c
    EQ -> c .|. c' .&. minBound
    GT -> c .|. minBound
  {-# INLINE setLS #-}

instance LSInt Int8 where
  isL = (< 0)
  {-# INLINE isL #-}
  isS = (>= 0)
  {-# INLINE isS #-}
  unLS = (.&. maxBound)
  {-# INLINE unLS #-}
  setLS c c' = case compare c (unLS c') of
    LT -> c
    EQ -> c .|. c' .&. minBound
    GT -> c .|. minBound
  {-# INLINE setLS #-}

instance LSInt Int16 where
  isL = (< 0)
  {-# INLINE isL #-}
  isS = (>= 0)
  {-# INLINE isS #-}
  unLS = (.&. maxBound)
  {-# INLINE unLS #-}
  setLS c c' = case compare c (unLS c') of
    LT -> c
    EQ -> c .|. c' .&. minBound
    GT -> c .|. minBound
  {-# INLINE setLS #-}

instance LSInt Int32 where
  isL = (< 0)
  {-# INLINE isL #-}
  isS = (>= 0)
  {-# INLINE isS #-}
  unLS = (.&. maxBound)
  {-# INLINE unLS #-}
  setLS c c' = case compare c (unLS c') of
    LT -> c
    EQ -> c .|. c' .&. minBound
    GT -> c .|. minBound
  {-# INLINE setLS #-}

instance LSInt Int64 where
  isL = (< 0)
  {-# INLINE isL #-}
  isS = (>= 0)
  {-# INLINE isS #-}
  unLS = (.&. maxBound)
  {-# INLINE unLS #-}
  setLS c c' = case compare c (unLS c') of
    LT -> c
    EQ -> c .|. c' .&. minBound
    GT -> c .|. minBound
  {-# INLINE setLS #-}

isLMS :: (LSInt a, LSInt b, U.Unbox a, U.Unbox b) => U.Vector a -> b -> Bool
isLMS ls si =
  si > 0
    && isL (U.unsafeIndex ls (fromIntegral si - 1))
    && isS (U.unsafeIndex ls (fromIntegral si))
{-# INLINE isLMS #-}

buildInitialBucket :: (LSInt a, U.Unbox a) => a -> U.Vector a -> U.Vector Int32
buildInitialBucket maxC ls =
  U.scanl' (+) 0 $
    U.unsafeAccumulate_
      (+)
      (U.replicate (fromIntegral maxC + 1) 0)
      (U.map (fromIntegral . unLS) ls)
      (U.replicate (U.length ls) 1)
{-# INLINE buildInitialBucket #-}

findLMSIndices :: (LSInt a, U.Unbox a) => U.Vector a -> U.Vector Int
findLMSIndices ls =
  U.filter (> 0) $
    U.izipWith
      ( \i pc c ->
          if isL pc && isS c
            then i + 1
            else 0
      )
      ls
      (U.tail ls)
{-# INLINE findLMSIndices #-}

induceSortL ::
  (LSInt a, LSInt b, U.Unbox a, U.Unbox b) =>
  UM.MVector s a ->
  UM.MVector s Int32 ->
  U.Vector b ->
  U.Vector Int32 ->
  ST s ()
induceSortL sa bucket ls bucket0 = do
  U.copy bucket (U.init bucket0)
  rep (U.length ls) $ \i -> do
    j <- subtract 1 . fromIntegral <$!> UM.unsafeRead sa i
    when (j >= 0) $ do
      let c = U.unsafeIndex ls j
      when (isL c) $ do
        pos <- UM.unsafeRead bucket (fromIntegral (unLS c))
        UM.unsafeWrite bucket (fromIntegral (unLS c)) (pos + 1)
        UM.unsafeWrite sa (fromIntegral pos) (fromIntegral j)
{-# INLINE induceSortL #-}

induceSortS ::
  (LSInt a, LSInt b, U.Unbox a, U.Unbox b) =>
  UM.MVector s a ->
  UM.MVector s Int32 ->
  U.Vector b ->
  U.Vector Int32 ->
  ST s ()
induceSortS sa bucket ls bucket0 = do
  U.copy bucket (U.tail bucket0)
  rev (U.length ls) $ \i -> do
    j <- subtract 1 . fromIntegral <$!> UM.unsafeRead sa i
    when (j >= 0) $ do
      let c = U.unsafeIndex ls j
      when (isS c) $ do
        pos <- subtract 1 <$!> UM.unsafeRead bucket (fromIntegral (unLS c))
        UM.unsafeWrite bucket (fromIntegral (unLS c)) pos
        UM.unsafeWrite sa (fromIntegral pos) (fromIntegral j)
{-# INLINE induceSortS #-}

reduceLMS ::
  (LSInt a, LSInt b, U.Unbox a, U.Unbox b) =>
  UM.MVector s a ->
  U.Vector b ->
  ST s (UM.MVector s a, a, U.Vector a)
reduceLMS sa ls = do
  !n1 <-
    U.foldM'
      ( \pos i -> do
          sj <- UM.unsafeRead sa i
          if isLMS ls sj
            then do
              UM.unsafeWrite sa pos sj
              return $! pos + 1
            else return pos
      )
      0
      (U.generate n id)

  UM.set (UM.drop n1 sa) (-1)
  !rank <-
    fst
      <$!> U.foldM'
        ( \(!r, !prev) i -> do
            cur <- fromIntegral <$!> UM.unsafeRead sa i
            let !r' = r + neqLMS ls prev cur
            UM.unsafeWrite sa (n1 + unsafeShiftR cur 1) r'
            return (r', cur)
        )
        (-1, 0)
        (U.generate n1 id)

  UM.write sa (n - 1) sentinelLS
  fix
    ( \loop !pos !i -> when (i >= n1) $ do
        r <- UM.unsafeRead sa i
        if r > 0
          then do
            r' <- UM.unsafeRead sa (pos + 1)
            UM.unsafeWrite sa pos (setLS r r')
            loop (pos - 1) (i - 1)
          else loop pos (i - 1)
    )
    (n - 2)
    (n1 + unsafeShiftR (n - 1) 1 - 1)

  (,,) (UM.take n1 sa) rank
    <$> U.unsafeFreeze (UM.drop (n - n1) sa)
  where
    !n = U.length ls

neqLMS :: (LSInt a, LSInt b, U.Unbox a) => U.Vector a -> Int -> Int -> b
neqLMS ls si sj
  | U.unsafeIndex ls si /= U.unsafeIndex ls sj = 1
  | otherwise = go 1
  where
    go !k
      | ci /= cj = 1
      | isS ci, isL (U.unsafeIndex ls (si + k - 1)) = 0
      | otherwise = go (k + 1)
      where
        ci = U.unsafeIndex ls (si + k)
        cj = U.unsafeIndex ls (sj + k)
{-# INLINE neqLMS #-}

sais ::
  (LSInt a, LSInt b, U.Unbox a, U.Unbox b) =>
  -- | filled with (-1)
  UM.MVector s b ->
  -- | the maximum alphabet
  a ->
  -- | LS typed
  U.Vector a ->
  ST s ()
sais msa _ ls | U.length ls == 1 = UM.write msa 0 0
sais msa maxC ls = do
  bkt <- U.thaw $ U.tail bucket0

  U.ifoldM'_
    ( \pc si c -> do
        if isL pc && isS c
          then do
            pos <- subtract 1 <$!> UM.unsafeRead bkt (fromIntegral (unLS c))
            UM.unsafeWrite bkt (fromIntegral (unLS c)) pos
            UM.unsafeWrite msa (fromIntegral pos) (fromIntegral si)
            return c
          else return c
    )
    sentinelLS
    ls

  induceSortL msa bkt ls bucket0
  induceSortS msa bkt ls bucket0

  (msa', maxC', ls') <- reduceLMS msa ls

  if fromIntegral maxC' < U.length ls' - 1
    then do
      UM.set msa' (-1)
      sais msa' maxC' ls'
    else do
      flip U.imapM_ ls' $ \i c -> do
        UM.unsafeWrite msa' (fromIntegral (unLS c)) (fromIntegral i)

  mls' <- U.unsafeThaw ls'
  U.imapM_ (\pos si -> UM.unsafeWrite mls' pos (fromIntegral si)) $
    findLMSIndices ls

  rep (UM.length msa') $ \i -> do
    UM.unsafeRead msa' i
      >>= UM.unsafeRead mls' . fromIntegral
      >>= UM.unsafeWrite msa' i . fromIntegral
  UM.set (UM.drop (UM.length msa') msa) (-1)

  U.copy bkt (U.tail bucket0)
  rev (UM.length msa') $ \i -> do
    !sj <- UM.unsafeRead msa' i
    UM.unsafeWrite msa' i (-1)
    let c = fromIntegral . unLS $ U.unsafeIndex ls (fromIntegral sj)
    pos <- subtract 1 <$!> UM.unsafeRead bkt c
    UM.unsafeWrite bkt c pos
    UM.unsafeWrite msa (fromIntegral pos) sj

  induceSortL msa bkt ls bucket0
  induceSortS msa bkt ls bucket0
  where
    !bucket0 = buildInitialBucket maxC ls
{-# SPECIALIZE sais :: UM.MVector s Int32 -> Int8 -> U.Vector Int8 -> ST s () #-}
{-# SPECIALIZE sais :: UM.MVector s Int32 -> Int32 -> U.Vector Int32 -> ST s () #-}
{-# SPECIALIZE sais :: UM.MVector s Int -> Int8 -> U.Vector Int8 -> ST s () #-}
{-# SPECIALIZE sais :: UM.MVector s Int -> Int -> U.Vector Int -> ST s () #-}
