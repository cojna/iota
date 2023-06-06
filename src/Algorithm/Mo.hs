{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 = Mo's Algotrithm
-}
module Algorithm.Mo where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Unsafe.Coerce

--

import Data.Vector.Sort.Radix (radixSort64)
import My.Prelude (stream, streamR)

-- | /O(N * sqrt Q)/
moAlgorithm ::
  (U.Unbox a, PrimMonad m) =>
  -- | index size (N)
  Int ->
  -- | query size (Q)
  Int ->
  -- | add
  (a -> Int -> m a) ->
  -- | delete
  (a -> Int -> m a) ->
  -- | initial value
  a ->
  -- | query [l, r)
  U.Vector MoQuery ->
  m (U.Vector a)
moAlgorithm n q add delete acc0 qs = do
  result <- UM.unsafeNew q
  U.foldM'_
    ( \(MoState l r acc) (MoQuery ql qr qi) -> do
        MS.foldM' add acc (streamR ql l)
          >>= flip (MS.foldM' add) (stream r qr)
          >>= flip (MS.foldM' delete) (streamR qr r)
          >>= flip (MS.foldM' delete) (stream l ql)
          >>= UM.unsafeWrite result qi
        MoState ql qr <$> UM.unsafeRead result qi
    )
    (MoState 0 0 acc0)
    (moSort (moBlockSize n q) qs)
  U.unsafeFreeze result
{-# INLINE moAlgorithm #-}

-- | @N\/sqrt\ Q@
moBlockSize :: Int -> Int -> Int
moBlockSize n q = ceiling @Double $ fromIntegral n / sqrt (fromIntegral q)

data MoState a
  = MoState
      !Int
      -- ^ left
      !Int
      -- ^ right
      !a

-- | query for [l,r)
data MoQuery
  = MoQuery
      !Int
      -- ^ left index (inclusive)
      !Int
      -- ^ right index (exclusive)
      !Int
      -- ^ query index

-- | radix sort
moSort ::
  -- | block size
  Int ->
  U.Vector MoQuery ->
  U.Vector MoQuery
moSort !blockSize qs =
  U.map (U.unsafeIndex qs . moDecodeQueryIndex)
    . radixSort64
    $ U.map (moEncode blockSize) qs
{-# INLINE moSort #-}

-- | query priority
moEncode ::
  -- | block size
  Int ->
  MoQuery ->
  Word64
moEncode !blockSize (MoQuery l r qi) =
  unsafeCoerce @Int @Word64 $
    unsafeShiftL l' 40 .|. unsafeShiftL r' 20 .|. qi
  where
    !l' = quot l blockSize
    !r'
      | l' .&. 1 == 1 = 0xfffff - r
      | otherwise = r
{-# INLINE moEncode #-}

moDecodeQueryIndex :: Word64 -> Int
moDecodeQueryIndex = unsafeCoerce @Word64 @Int . (.&. 0xfffff)
{-# INLINE moDecodeQueryIndex #-}

encodeMoQuery :: MoQuery -> Word64
encodeMoQuery (MoQuery l r i) = fromIntegral $ unsafeShiftL l 40 .|. unsafeShiftL r 20 .|. i
{-# INLINE encodeMoQuery #-}

decodeMoQuery :: Word64 -> MoQuery
decodeMoQuery w =
  MoQuery
    (unsafeShiftR (fromIntegral w) 40)
    (unsafeShiftR (fromIntegral w) 20 .&. 0xfffff)
    (fromIntegral w .&. 0xfffff)
{-# INLINE decodeMoQuery #-}

newtype instance UM.MVector s MoQuery = MV_MoQuery (UM.MVector s Word64)
newtype instance U.Vector MoQuery = V_MoQuery (U.Vector Word64)
instance U.Unbox MoQuery
instance GM.MVector UM.MVector MoQuery where
  basicLength (MV_MoQuery v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_MoQuery v) = MV_MoQuery $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_MoQuery v1) (MV_MoQuery v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_MoQuery `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_MoQuery v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_MoQuery `liftM` GM.basicUnsafeReplicate n (encodeMoQuery x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_MoQuery v) i = decodeMoQuery `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_MoQuery v) i x = GM.basicUnsafeWrite v i (encodeMoQuery x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_MoQuery v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_MoQuery v) x = GM.basicSet v (encodeMoQuery x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_MoQuery v1) (MV_MoQuery v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_MoQuery v1) (MV_MoQuery v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_MoQuery v) n = MV_MoQuery `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance G.Vector U.Vector MoQuery where
  basicUnsafeFreeze (MV_MoQuery v) = V_MoQuery `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_MoQuery v) = MV_MoQuery `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_MoQuery v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_MoQuery v) = V_MoQuery $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_MoQuery v) i = decodeMoQuery `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_MoQuery mv) (V_MoQuery v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
