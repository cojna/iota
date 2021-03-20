{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Data.Mat3x3 where

#if !MIN_VERSION_primitive(7,0,0)
import Control.Monad (zipWithM_)
#endif
import Control.Monad.ST
import Data.Function
import Data.Primitive
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts

data Mat3x3 a = Mat3x3 !Int !ByteArray

instance (Prim a, Eq a) => Eq (Mat3x3 a) where
    (==) = (==) `on` toList

instance (Prim a, Show a) => Show (Mat3x3 a) where
    show = show . toList

instance (Prim a) => IsList (Mat3x3 a) where
    type Item (Mat3x3 a) = a
#if MIN_VERSION_primitive(7,0,0)
    fromList = Mat3x3 0 . byteArrayFromListN 9
#else
    fromList xs = createMat3x3 $ \mba -> do
        zipWithM_ (writeByteArray mba) [0..] xs
#endif
    toList (Mat3x3 o ba) = map (indexByteArray ba) [o .. o + 8]

appMat3x3 :: (Prim a, Num a) => Mat3x3 a -> a -> a -> a -> (a, a, a)
appMat3x3 (Mat3x3 o ba) x y z = (x', y', z')
  where
    !x' =
        x * indexByteArray ba (o + 0)
            + y * indexByteArray ba (o + 1)
            + z * indexByteArray ba (o + 2)
    !y' =
        x * indexByteArray ba (o + 3)
            + y * indexByteArray ba (o + 4)
            + z * indexByteArray ba (o + 5)
    !z' =
        x * indexByteArray ba (o + 6)
            + y * indexByteArray ba (o + 7)
            + z * indexByteArray ba (o + 8)

rep3 :: (Int -> ST s ()) -> ST s ()
rep3 f = f 0 *> f 1 *> f 2
{-# INLINE rep3 #-}

rep9 :: (Int -> ST s ()) -> ST s ()
rep9 f = f 0 *> f 1 *> f 2 *> f 3 *> f 4 *> f 5 *> f 6 *> f 7 *> f 8
{-# INLINE rep9 #-}

createMat3x3 :: forall a. (Prim a) => (forall s. MutableByteArray s -> ST s ()) -> Mat3x3 a
createMat3x3 run = runST $ do
    mba <- newByteArray (sizeOf @a undefined * 9)
    run mba
    Mat3x3 0 <$> unsafeFreezeByteArray mba

genMat3x3 :: (Prim a) => (Int -> Int -> a) -> Mat3x3 a
genMat3x3 f = createMat3x3 $ \mba -> do
    rep3 $ \i -> do
        rep3 $ \j -> do
            writeByteArray mba (3 * i + j) (f i j)

instance (Prim a, Num a) => Num (Mat3x3 a) where
    (Mat3x3 ox xs) + (Mat3x3 oy ys) = createMat3x3 $ \mba -> rep9 $ \i -> do
        writeByteArray @a mba i $
            indexByteArray xs (ox + i) + indexByteArray ys (oy + i)
    (Mat3x3 ox xs) - (Mat3x3 oy ys) = createMat3x3 $ \mba -> rep9 $ \i -> do
        writeByteArray @a mba i $
            indexByteArray xs (ox + i) - indexByteArray ys (oy + i)
    (Mat3x3 ox xs) * (Mat3x3 oy ys) = createMat3x3 $ \mba -> do
        rep3 $ \i -> do
            let !ox' = ox + 3 * i
            rep3 $ \j -> do
                let !oy' = oy + j
                writeByteArray @a mba (3 * i + j) $
                    indexByteArray xs ox' * indexByteArray ys oy'
                        + indexByteArray xs (ox' + 1) * indexByteArray ys (oy' + 3)
                        + indexByteArray xs (ox' + 2) * indexByteArray ys (oy' + 6)
    negate (Mat3x3 ox xs) = createMat3x3 $ \mba -> rep9 $ \i -> do
        writeByteArray @a mba i . negate $
            indexByteArray xs (ox + i)
    abs = id
    signum = const 1
    fromInteger x = createMat3x3 $ \mba -> do
        setByteArray @a mba 0 9 0
        writeByteArray @a mba 0 (fromIntegral x)
        writeByteArray @a mba 4 (fromIntegral x)
        writeByteArray @a mba 8 (fromIntegral x)

data instance UM.MVector s (Mat3x3 a) = MV_Mat3x3 !Int !Int !(MutableByteArray s)
data instance U.Vector (Mat3x3 a) = V_Mat3x3 !Int !Int !ByteArray

instance (Prim a) => U.Unbox (Mat3x3 a)

instance (Prim a) => GM.MVector UM.MVector (Mat3x3 a) where
    basicLength (MV_Mat3x3 _ n _) = quot n 9
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Mat3x3 o _ mba) = MV_Mat3x3 (o + 9 * i) (9 * n) mba
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Mat3x3 ox nx xs) (MV_Mat3x3 oy ny ys) =
        sameMutableByteArray xs ys
            && ox < oy + ny
            && oy < ox + nx
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Mat3x3 0 (9 * n) <$> newByteArray (sizeOf @a undefined * 9 * n)
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Mat3x3 o n mba) = fillByteArray mba (sz * o) (sz * n) 0
      where
        sz = sizeOf @a undefined
    {-# INLINE basicInitialize #-}
    basicUnsafeRead (MV_Mat3x3 o n mba) i = do
        dst <- newByteArray (sz * 9)
        copyMutableByteArray dst 0 mba (sz * o) (sz * 9)
        Mat3x3 0 <$> unsafeFreezeByteArray dst
      where
        sz = sizeOf @a undefined
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Mat3x3 o _ mba) i (Mat3x3 o' ba) =
        copyByteArray mba (sz * o) ba (sz * o') (sz * 9)
      where
        sz = sizeOf @a undefined
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeCopy (MV_Mat3x3 o n dst) (MV_Mat3x3 o' n' src) =
        copyMutableByteArray dst (sz * o) src (sz * o') (sz * n')
      where
        sz = sizeOf @a undefined
    {-# INLINE basicUnsafeCopy #-}

instance (Prim a) => G.Vector U.Vector (Mat3x3 a) where
    basicUnsafeFreeze (MV_Mat3x3 o n mba) = V_Mat3x3 o n <$> unsafeFreezeByteArray mba
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Mat3x3 o n ba) = MV_Mat3x3 o n <$> unsafeThawByteArray ba
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Mat3x3 _ n _) = quot n 9
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Mat3x3 o _ ba) = V_Mat3x3 (o + 9 * i) (9 * n) ba
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Mat3x3 o n ba) i = return $! Mat3x3 (o + 9 * i) ba
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Mat3x3 o n dst) (V_Mat3x3 o' n' src) =
        copyByteArray dst (sz * o) src (sz * o') (sz * n')
      where
        sz = sizeOf @a undefined
    elemseq _ = seq
    {-# INLINE elemseq #-}
