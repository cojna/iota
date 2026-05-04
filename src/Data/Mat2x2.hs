{-# LANGUAGE TypeFamilies #-}

module Data.Mat2x2 where

import Data.Primitive
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data Mat2x2 a = Mat2x2 !a !a !a !a
  deriving (Eq, Ord, Show)

appMat2x2 :: (Num a) => Mat2x2 a -> a -> a -> (a, a)
appMat2x2 (Mat2x2 a b c d) x y = (x', y')
  where
    !x' = a * x + b * y
    !y' = c * x + d * y

instance (Num a) => Num (Mat2x2 a) where
  (Mat2x2 a0 b0 c0 d0) + (Mat2x2 a1 b1 c1 d1) =
    Mat2x2 (a0 + a1) (b0 + b1) (c0 + c1) (d0 + d1)
  (Mat2x2 a0 b0 c0 d0) - (Mat2x2 a1 b1 c1 d1) =
    Mat2x2 (a0 - a1) (b0 - b1) (c0 - c1) (d0 - d1)
  (Mat2x2 a0 b0 c0 d0) * (Mat2x2 a1 b1 c1 d1) =
    Mat2x2
      ((a0 * a1) + (b0 * c1))
      ((a0 * b1) + (b0 * d1))
      ((c0 * a1) + (d0 * c1))
      ((c0 * b1) + (d0 * d1))
  negate (Mat2x2 a b c d) = Mat2x2 (negate a) (negate b) (negate c) (negate d)
  abs = id
  signum = const 1
  fromInteger x = Mat2x2 (fromInteger x) 0 0 (fromInteger x)

data instance UM.MVector s (Mat2x2 a) = MV_Mat2x2 !Int !Int !(MutableByteArray s)
data instance U.Vector (Mat2x2 a) = V_Mat2x2 !Int !Int !ByteArray

instance (Prim a) => U.Unbox (Mat2x2 a)

instance (Prim a) => GM.MVector UM.MVector (Mat2x2 a) where
  basicLength (MV_Mat2x2 _ n _) = n
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Mat2x2 o _ mba) = MV_Mat2x2 (o + i) n mba
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Mat2x2 ox nx xs) (MV_Mat2x2 oy ny ys) =
    sameMutableByteArray xs ys
      && ox < oy + ny
      && oy < ox + nx
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Mat2x2 0 n <$> newByteArray (sizeOf @a undefined * 4 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Mat2x2 o n mba) = fillByteArray mba (sz * 4 * o) (sz * 4 * n) 0
    where
      sz = sizeOf @a undefined
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_Mat2x2 o _ mba) i = do
    Mat2x2
      <$> readByteArray mba (4 * (o + i))
      <*> readByteArray mba (4 * (o + i) + 1)
      <*> readByteArray mba (4 * (o + i) + 2)
      <*> readByteArray mba (4 * (o + i) + 3)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Mat2x2 o _ mba) i (Mat2x2 m00 m01 m10 m11) = do
    writeByteArray mba (4 * (o + i)) m00
    writeByteArray mba (4 * (o + i) + 1) m01
    writeByteArray mba (4 * (o + i) + 2) m10
    writeByteArray mba (4 * (o + i) + 3) m11
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeCopy (MV_Mat2x2 o n dst) (MV_Mat2x2 o' _ src) =
    copyMutableByteArray dst (sz * 4 * o) src (sz * 4 * o') (sz * 4 * n)
    where
      sz = sizeOf @a undefined
  {-# INLINE basicUnsafeCopy #-}

instance (Prim a) => G.Vector U.Vector (Mat2x2 a) where
  basicUnsafeFreeze (MV_Mat2x2 o n mba) = V_Mat2x2 o n <$> unsafeFreezeByteArray mba
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Mat2x2 o n ba) = MV_Mat2x2 o n <$> unsafeThawByteArray ba
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Mat2x2 _ n _) = n
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Mat2x2 o _ ba) = V_Mat2x2 (o + i) n ba
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Mat2x2 o _ ba) i =
    return $!
      Mat2x2
        (indexByteArray ba (4 * (o + i)))
        (indexByteArray ba (4 * (o + i) + 1))
        (indexByteArray ba (4 * (o + i) + 2))
        (indexByteArray ba (4 * (o + i) + 3))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Mat2x2 o n dst) (V_Mat2x2 o' _ src) =
    copyByteArray dst (sz * 4 * o) src (sz * 4 * o') (sz * 4 * n)
    where
      sz = sizeOf @a undefined
  elemseq _ = seq
  {-# INLINE elemseq #-}
