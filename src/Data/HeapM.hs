{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.HeapM where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

heapify :: (PrimMonad m, GM.MVector mv a)
    => (a -> a -> Ordering) -> mv (PrimState m) a ->  m ()
heapify cmp vec =
    flip fix (unsafeShiftR (GM.length vec - 1) 1) $ \loop !i ->
      when (i >= 0) $ do
        siftDown cmp i vec
        loop $ i - 1
{-# INLINE heapify #-}

siftDown :: (PrimMonad m, GM.MVector mv a)
    => (a -> a -> Ordering) -> Int -> mv (PrimState m) a -> m ()
siftDown cmp offset vec = do
    let !len = GM.length vec
    flip fix offset $ \loop !parent -> do
        let !l = unsafeShiftL parent 1 + 1
            !r = l + 1
        x <- GM.unsafeRead vec parent
        when (l < len) $ do
            childL <- GM.unsafeRead vec l
            if r < len
            then do
                childR <- GM.unsafeRead vec r
                case cmp childL childR of
                    LT -> when (cmp x childL == GT) $ do
                        GM.unsafeSwap vec parent l
                        loop l
                    _ -> when (cmp x childR == GT) $ do
                        GM.unsafeSwap vec parent r
                        loop r
            else when (cmp x childL == GT) $ do
                GM.unsafeSwap vec parent l
                loop l
{-# INLINE siftDown #-}

