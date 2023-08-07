{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 = Mo's Algotrithm
-}
module Algorithm.Mo where

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
import My.Prelude ((..<), (>..))

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
        MS.foldM' add acc (l >.. ql)
          >>= flip (MS.foldM' add) (r ..< qr)
          >>= flip (MS.foldM' delete) (r >.. qr)
          >>= flip (MS.foldM' delete) (l ..< ql)
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

instance U.IsoUnbox MoQuery Word64 where
  toURepr = encodeMoQuery
  {-# INLINE toURepr #-}
  fromURepr = decodeMoQuery
  {-# INLINE fromURepr #-}

newtype instance UM.MVector s MoQuery = MV_MoQuery (UM.MVector s Word64)
newtype instance U.Vector MoQuery = V_MoQuery (U.Vector Word64)
deriving via (MoQuery `U.As` Word64) instance GM.MVector U.MVector MoQuery
deriving via (MoQuery `U.As` Word64) instance G.Vector U.Vector MoQuery
instance U.Unbox MoQuery
