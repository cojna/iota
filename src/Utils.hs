{-# LANGUAGE BangPatterns #-}

module Utils where

import           Data.Bits
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Unsafe.Coerce

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n = U.forM_ $ U.generate n id
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
{-# INLINE rev #-}

floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)
