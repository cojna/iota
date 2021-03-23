{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

{- |
 = Mo's Algotrithm
-}
module Algorithm.Mo where

import Control.Monad.Primitive
import Data.Bits
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Unsafe.Coerce

--

import Data.Vector.Sort.Radix (radixSort64)
import My.Prelude (stream, streamR)

-- | /O((N+Q)sqrt N)/
moAlgorithm ::
  (U.Unbox a, PrimMonad m) =>
  -- | add
  (a -> Int -> m a) ->
  -- | delete
  (a -> Int -> m a) ->
  -- | initial value
  a ->
  -- | block size (sqrt N)
  Int ->
  -- | query [l, r)
  U.Vector (Int, Int) ->
  m (U.Vector a)
moAlgorithm add delete acc0 blockSize lrs = do
  result <- UM.unsafeNew (U.length lrs)
  U.foldM'_
    ( \(MoState l r acc) (qi, (ql, qr)) -> do
        !addR <- MS.foldM' add acc $ stream r qr
        !deleteR <- MS.foldM' delete addR $ streamR qr r
        !addL <- MS.foldM' add deleteR $ streamR ql l
        !deleteL <- MS.foldM' delete addL $ stream l ql
        UM.unsafeWrite result qi deleteL
        return $! MoState ql qr deleteL
    )
    (MoState 0 0 acc0)
    (moSort blockSize lrs)
  U.unsafeFreeze result
{-# INLINE moAlgorithm #-}

moBlockSize :: Int -> Int
moBlockSize = ceiling . sqrt @Double . fromIntegral

data MoState a = MoState !Int !Int !a deriving (Eq)

moSort :: Int -> U.Vector (Int, Int) -> U.Vector (Int, (Int, Int))
moSort blockSize lrs =
  U.map (\i -> (i, U.unsafeIndex lrs i))
    . U.map moDecode
    . radixSort64
    $ U.imap (\i (l, r) -> moEncode blockSize i l r) lrs
{-# INLINE moSort #-}

moEncode :: Int -> Int -> Int -> Int -> Word64
moEncode blockSize qi l r =
  unsafeCoerce @Int @Word64 $
    unsafeShiftL l' 40 .|. unsafeShiftL r' 20 .|. qi
  where
    l' = quot l blockSize
    r'
      | l' .&. 1 == 1 = 0xfffff - r
      | otherwise = r
{-# INLINE moEncode #-}

moDecode :: Word64 -> Int
moDecode = unsafeCoerce @Word64 @Int . (.&. 0xfffff)
{-# INLINE moDecode #-}
