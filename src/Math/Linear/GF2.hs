{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Linear.GF2 where

import Control.Monad
import Data.Bits
import Data.Coerce (coerce)
import qualified Data.List as L
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |
\(GF(2)^{64}\)

bitwise xor

+---+---+---+
| + | 0 | 1 |
+---+---+---+
| 0 | 0 | 1 |
+---+---+---+
| 1 | 1 | 0 |
+---+---+---+

bitwise and

+---+---+---+
| * | 0 | 1 |
+---+---+---+
| 0 | 0 | 0 |
+---+---+---+
| 1 | 0 | 1 |
+---+---+---+
-}
newtype GF2x64 = GF2x64 {getGF2x64 :: Word}
  deriving newtype (Eq, Ord, Bits, FiniteBits)

{- |
>>> show (GF2x64 10)
"0b1010"
>>> show (GF2x64 0)
"0b0"
-}
instance Show GF2x64 where
  show (GF2x64 0) = "0b0"
  show (GF2x64 x0) = "0b" ++ reverse (L.unfoldr next x0)
    where
      next :: Word -> Maybe (Char, Word)
      next 0 = Nothing
      next x
        | testBit x 0 = Just ('1', unsafeShiftR x 1)
        | otherwise = Just ('0', unsafeShiftR x 1)

-- | subspace of \(GF(2)^{64}\)
newtype GF2x64' = GF2x64' {basisGF2x64' :: U.Vector GF2x64}
  deriving newtype (Show)

zeroGF2x64' :: GF2x64'
zeroGF2x64' = GF2x64' U.empty

dimGF2x64' :: GF2x64' -> Int
dimGF2x64' = U.length . basisGF2x64'

insertGF2x64' :: GF2x64 -> GF2x64' -> GF2x64'
insertGF2x64' v (GF2x64' bs)
  | v' == GF2x64 0 = GF2x64' bs
  | otherwise = GF2x64' (bs `U.snoc` v')
  where
    v' = U.foldl' (\x b -> min (xor x b) x) v bs

spanGF2x64' :: U.Vector GF2x64 -> GF2x64'
spanGF2x64' = U.foldl' (flip insertGF2x64') zeroGF2x64'

{- |
>>> :set -XBinaryLiterals
>>> sp = spanGF2x64' $ U.fromList $ map GF2x64 [0b11, 0b01]
>>> componentsGF2x64' sp (GF2x64 0b10)
0b11
-}
componentsGF2x64' :: GF2x64' -> GF2x64 -> GF2x64
componentsGF2x64' (GF2x64' basis) v0 =
  case U.ifoldl' step (GF2x64 0, v0) basis of
    (res, GF2x64 0) -> res
    (res, _) -> setBit res (U.length basis)
  where
    step :: (GF2x64, GF2x64) -> Int -> GF2x64 -> (GF2x64, GF2x64)
    step (!acc, !v) i base
      | v' < v = (setBit acc i, v')
      | otherwise = (acc, v)
      where
        v' = xor v base

newtype instance UM.MVector s GF2x64 = MV_GF2x64 (UM.MVector s Word)
newtype instance U.Vector GF2x64 = V_GF2x64 (U.Vector Word)
instance U.Unbox GF2x64
instance GM.MVector UM.MVector GF2x64 where
  basicLength (MV_GF2x64 v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_GF2x64 v) = MV_GF2x64 $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_GF2x64 v1) (MV_GF2x64 v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_GF2x64 `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_GF2x64 v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_GF2x64 `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_GF2x64 v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_GF2x64 v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_GF2x64 v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_GF2x64 v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_GF2x64 v1) (MV_GF2x64 v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_GF2x64 v1) (MV_GF2x64 v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_GF2x64 v) n = MV_GF2x64 `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance G.Vector U.Vector GF2x64 where
  basicUnsafeFreeze (MV_GF2x64 v) = V_GF2x64 `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_GF2x64 v) = MV_GF2x64 `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_GF2x64 v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_GF2x64 v) = V_GF2x64 $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_GF2x64 v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_GF2x64 mv) (V_GF2x64 v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
