{-# LANGUAGE TypeFamilies #-}

module Data.ByteString.Tiny where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import GHC.Exts

{- | at most 16 bytes

>>> compare (packTBS "a") (packTBS "b")
LT
>>> compare (packTBS "a") (packTBS "ab")
LT
-}
data TinyByteString = TBS !Word64 !Word64
  deriving (Eq, Ord)

instance Show TinyByteString where
  show = unpackTBS
  {-# INLINE show #-}

instance IsString TinyByteString where
  fromString = packTBS
  {-# INLINE fromString #-}

toTiny :: B.ByteString -> TinyByteString
toTiny bs = TBS (pack bs) (pack $ B.drop 8 bs)
  where
    pack s =
      F.foldl' (.|.) 0 $
        zipWith
          (unsafeShiftL . fromIntegral)
          (B.unpack s)
          [56, 48, 40, 32, 24, 16, 8, 0]

{- |
>>> lengthTBS (packTBS "")
0
>>> lengthTBS (packTBS $ take 16 ['a'..])
16
-}
lengthTBS :: TinyByteString -> Int
lengthTBS (TBS bs0 bs1) = len bs0 + len bs1
  where
    len bs = 8 - unsafeShiftR (countTrailingZeros bs) 3

packTBS :: String -> TinyByteString
packTBS cs = TBS (pack cs) (pack $ drop 8 cs)
  where
    pack s =
      F.foldl' (.|.) 0 $
        zipWith
          (unsafeShiftL . fromIntegral . ord)
          s
          [56, 48, 40, 32, 24, 16, 8, 0]

unpackTBS :: TinyByteString -> String
unpackTBS (TBS bs0 bs1) =
  takeWhile (/= '\0') $
    unpack bs0 <> unpack bs1
  where
    unpack bs =
      map
        (chr . fromIntegral . (.&. 0x7f) . unsafeShiftR bs)
        [56, 48, 40, 32, 24, 16, 8, 0]

newtype instance UM.MVector s TinyByteString = MV_TinyByteString (UM.MVector s Word64)
newtype instance U.Vector TinyByteString = V_TinyByteString (U.Vector Word64)

instance U.Unbox TinyByteString

instance GM.MVector UM.MVector TinyByteString where
  basicLength (MV_TinyByteString v) = unsafeShiftR (GM.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_TinyByteString v) = MV_TinyByteString $ GM.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_TinyByteString v1) (MV_TinyByteString v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_TinyByteString `liftM` GM.basicUnsafeNew (2 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_TinyByteString v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_TinyByteString v) i = liftM2 TBS (GM.basicUnsafeRead v (2 * i)) (GM.basicUnsafeRead v (2 * i + 1))
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_TinyByteString v) i (TBS x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_TinyByteString v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicUnsafeCopy (MV_TinyByteString v1) (MV_TinyByteString v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_TinyByteString v1) (MV_TinyByteString v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_TinyByteString v) n = MV_TinyByteString `liftM` GM.basicUnsafeGrow v (2 * n)
  {-# INLINE basicUnsafeGrow #-}

instance G.Vector U.Vector TinyByteString where
  basicUnsafeFreeze (MV_TinyByteString v) = V_TinyByteString `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_TinyByteString v) = MV_TinyByteString `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_TinyByteString v) = unsafeShiftR (G.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_TinyByteString v) = V_TinyByteString $ G.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_TinyByteString v) i = liftM2 TBS (G.basicUnsafeIndexM v (2 * i)) (G.basicUnsafeIndexM v (2 * i + 1))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_TinyByteString mv) (V_TinyByteString v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
