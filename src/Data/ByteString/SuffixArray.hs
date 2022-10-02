{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Data.ByteString.SuffixArray where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Function
import Data.Int
import Data.Primitive
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Primitive.Mutable as PVM
import Data.Word

import My.Prelude

{- |
>>> :set -XOverloadedStrings
>>> buildSuffixArray "aaa"
[3,2,1,0]
>>> buildSuffixArray "mississippi"
[11,10,7,4,1,0,9,8,6,3,5,2]
>>> buildSuffixArray "abracadabra"
[11,10,7,0,3,5,8,1,4,6,9,2]
>>> buildSuffixArray "ababab"
[6,4,2,0,5,3,1]
-}
data SuffixArray = SuffixArray !Int !ByteArray

instance Show SuffixArray where
  show (SuffixArray n ba) = show $ map (indexByteArray @Int32 ba) [0 .. n - 1]

indexSA :: SuffixArray -> Int -> Int
indexSA (SuffixArray _ ba) = fromIntegral @Int32 . indexByteArray ba
{-# INLINE indexSA #-}

buildSuffixArray :: B.ByteString -> SuffixArray
buildSuffixArray bs = runST $ do
  sa <- newPinnedByteArray (4 * n)
  mbuf <- PVM.MVector 0 n <$> newPinnedByteArray (4 * n)
  PVM.unsafeWrite mbuf (n - 1) (setLMSFlag 0)
  rev (n - 1) $ \i ->
    writeLSInfo mbuf i . fromIntegral $ B.unsafeIndex bs i
  sais 127 mbuf (PVM.MVector 0 n sa)
  SuffixArray n <$!> unsafeFreezeByteArray sa
  where
    !n = B.length bs + 1

sais ::
  Word32 -> PVM.MVector s Word32 -> PVM.MVector s Int32 -> ST s ()
sais !maxC cs sa@(PVM.MVector o n bufSA) = do
  fillByteArray bufSA (4 * o) (4 * n) 0xff

  !bucket0 <- buildBucketSA maxC cs

  bucket <- PV.thaw $ PV.tail bucket0
  rep n $ \i -> do
    lsc <- PVM.unsafeRead cs i
    when (isLMS lsc) $ do
      let c = viewC lsc
      pos <- subtract 1 <$!> PVM.unsafeRead bucket c
      PVM.unsafeWrite bucket c pos
      PVM.unsafeWrite sa pos $ fromIntegral @_ @Int32 i

  induceSortL cs sa bucket0 bucket
  induceSortS cs sa bucket0 bucket

  (maxC', ls', sa') <- compressLMS cs sa

  let !n1 = PVM.length ls'

  if fromIntegral maxC' < n1 - 1
    then sais maxC' ls' sa'
    else do
      rep n1 $ \i -> do
        c <- viewC <$> PVM.unsafeRead ls' i
        PVM.unsafeWrite sa' c (fromIntegral @_ @Int32 i)

  PV.foldM'_
    ( \pos i -> do
        lcs <- PVM.unsafeRead cs i
        if isLMS lcs
          then do
            PVM.unsafeWrite ls' pos (fromIntegral i)
            return $ pos + 1
          else return pos
    )
    0
    (PV.generate n id)

  rep n1 $ \i -> do
    PVM.unsafeRead sa' i
      >>= PVM.unsafeRead ls' . fromIntegral @Int32
      >>= PVM.unsafeWrite sa' i . fromIntegral @Word32 @Int32

  fillByteArray bufSA (4 * (o + n1)) (4 * (n - n1)) 0xff
  PV.copy bucket (PV.tail bucket0)
  rev n1 $ \i -> do
    !j <- PVM.unsafeRead sa' i
    PVM.unsafeWrite sa' i (-1)
    c <- viewC <$!> PVM.unsafeRead cs (fromIntegral @Int32 j)
    pos <- subtract 1 <$!> PVM.unsafeRead bucket c
    PVM.unsafeWrite bucket c pos
    PVM.unsafeWrite sa pos j

  induceSortL cs sa bucket0 bucket
  induceSortS cs sa bucket0 bucket

writeLSInfo :: PVM.MVector s Word32 -> Int -> Word32 -> ST s ()
writeLSInfo cs i cur = do
  next <- PVM.unsafeRead cs (i + 1)
  case compare (viewC cur) (viewC next) of
    LT -> PVM.unsafeWrite cs i $ setSFlag cur
    EQ -> PVM.unsafeWrite cs i $ cur .|. maskLSFlag next
    GT -> do
      PVM.unsafeWrite cs i cur
      when (isS next) $ do
        PVM.unsafeWrite cs (i + 1) $ setLMSFlag next
{-# INLINE writeLSInfo #-}

setSFlag :: Word32 -> Word32
setSFlag = (.|. 0x40000000)
{-# INLINE setSFlag #-}

setLFlag :: Word32 -> Word32
setLFlag = (.&. complement 0x40000000)
{-# INLINE setLFlag #-}

setLMSFlag :: Word32 -> Word32
setLMSFlag = (.|. 0xc0000000)
{-# INLINE setLMSFlag #-}

maskLSFlag :: Word32 -> Word32
maskLSFlag = (.&. 0x40000000)
{-# INLINE maskLSFlag #-}

isS :: Word32 -> Bool
isS = (> 0x40000000)
{-# INLINE isS #-}

isL :: Word32 -> Bool
isL = (< 0x40000000)
{-# INLINE isL #-}

isLMS :: Word32 -> Bool
isLMS = (> 0x80000000)
{-# INLINE isLMS #-}

viewC :: Word32 -> Int
viewC = fromIntegral . (.&. 0x3fffffff)
{-# INLINE viewC #-}

buildBucketSA :: Word32 -> PVM.MVector s Word32 -> ST s (PV.Vector Int)
buildBucketSA maxC cs = do
  bkt <- PVM.replicate (fromIntegral @Word32 maxC + 2) 0
  rep (PVM.length cs) $ \i -> do
    c <- viewC <$> PVM.unsafeRead cs i
    PVM.unsafeModify bkt (+ 1) (c + 1)
  rep (fromIntegral @Word32 maxC + 1) $ \i -> do
    cnt <- PVM.unsafeRead bkt i
    PVM.unsafeModify bkt (+ cnt) (i + 1)
  PV.unsafeFreeze bkt

induceSortL ::
  PVM.MVector s Word32 ->
  PVM.MVector s Int32 ->
  PV.Vector Int ->
  PVM.MVector s Int ->
  ST s ()
induceSortL cs sa bucket0 bucket = do
  PV.copy bucket $ PV.init bucket0
  rep (PVM.length cs) $ \i -> do
    j <- subtract 1 <$!> PVM.unsafeRead sa i
    when (j >= 0) $ do
      lcs <- PVM.unsafeRead cs $ fromIntegral @Int32 @Int j
      when (isL lcs) $ do
        let !c = viewC lcs
        pos <- PVM.unsafeRead bucket c
        PVM.unsafeWrite bucket c (pos + 1)
        PVM.unsafeWrite sa pos j
{-# INLINE induceSortL #-}

induceSortS ::
  PVM.MVector s Word32 ->
  PVM.MVector s Int32 ->
  PV.Vector Int ->
  PVM.MVector s Int ->
  ST s ()
induceSortS cs sa bucket0 bucket = do
  PV.copy bucket $ PV.tail bucket0
  rev (PVM.length cs) $ \i -> do
    j <- subtract 1 <$!> PVM.unsafeRead sa i
    when (j >= 0) $ do
      lsc <- PVM.unsafeRead cs $ fromIntegral @Int32 @Int j
      when (isS lsc) $ do
        let !c = viewC lsc
        pos <- subtract 1 <$!> PVM.unsafeRead bucket c
        PVM.unsafeWrite bucket c pos
        PVM.unsafeWrite sa pos j
{-# INLINE induceSortS #-}

compressLMS ::
  PVM.MVector s Word32 ->
  PVM.MVector s Int32 ->
  ST s (Word32, PVM.MVector s Word32, PVM.MVector s Int32)
compressLMS cs sa@(PVM.MVector o n bufSA) = do
  !n1 <-
    PV.foldM'
      ( \pos i -> do
          sai <- PVM.unsafeRead sa i
          lsc <- PVM.unsafeRead cs $ fromIntegral @Int32 @Int sai
          if isLMS lsc
            then do
              PVM.unsafeWrite sa pos sai
              return $! pos + 1
            else return pos
      )
      0
      (PV.generate n id)

  fillByteArray bufSA (4 * (o + n1)) (4 * (n - n1)) 0xff
  rank <-
    fst
      <$!> PV.foldM'
        ( \(!r, !prev) i -> do
            cur <- fromIntegral @Int32 <$> PVM.unsafeRead sa i
            !r' <- (+ r) <$> eqLMS prev cur
            PVM.unsafeWrite sa (n1 + unsafeShiftR cur 1) r'
            return (r', cur)
        )
        (-1, 0)
        (PV.generate n1 id)
  PVM.write sa (n - 1) (fromIntegral @Word32 @Int32 $ setLMSFlag 0)
  fix
    ( \loop !pos !i -> when (i >= n1) $ do
        r <- PVM.unsafeRead sa i
        if r > 0
          then do
            writeLSInfo (PVM.MVector o n bufSA) pos $ fromIntegral @Int32 @Word32 r
            loop (pos - 1) (i - 1)
          else loop pos (i - 1)
    )
    (n - 2)
    (n1 + unsafeShiftR (n - 1) 1 - 1)
  return
    ( fromIntegral @Int32 rank
    , PVM.MVector (o + n - n1) n1 bufSA
    , PVM.take n1 sa
    )
  where
    eqLMS x y = do
      cx0 <- PVM.unsafeRead cs x
      cy0 <- PVM.unsafeRead cs y
      if cx0 == cy0
        then go 1
        else return 1
      where
        go !i = do
          cx <- PVM.unsafeRead cs (x + i)
          cy <- PVM.unsafeRead cs (y + i)
          if cx == cy
            then
              if isLMS cx
                then return 0
                else go (i + 1)
            else return 1
    {-# INLINE eqLMS #-}
{-# INLINE compressLMS #-}
