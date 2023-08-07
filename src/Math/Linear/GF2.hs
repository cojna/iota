{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Linear.GF2 where

import Data.Bits
import qualified Data.List as L
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Exts (IsList (Item, fromList, toList))

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

instance Num GF2x64 where
  (+) = xor
  (-) = xor
  (*) = (.&.)
  negate = id
  abs = id
  signum = const (complement zeroBits)
  fromInteger = GF2x64 . fromInteger

-- | subspace of \(GF(2)^{64}\)
newtype GF2x64' = GF2x64' {basisGF2x64' :: U.Vector GF2x64}
  deriving newtype (Show)

instance Eq GF2x64' where
  xs == ys =
    rankGF2x64' xs == rankGF2x64' ys
      && U.all (`inGF2x64'` xs) (basisGF2x64' ys)

instance IsList GF2x64' where
  type Item GF2x64' = GF2x64
  fromList = spanGF2x64' . U.fromList
  toList = U.toList . basisGF2x64'

zeroGF2x64' :: GF2x64'
zeroGF2x64' = GF2x64' U.empty

-- | /O(1)/
rankGF2x64' :: GF2x64' -> Int
rankGF2x64' = U.length . basisGF2x64'

-- | /O(d)/
inGF2x64' :: GF2x64 -> GF2x64' -> Bool
inGF2x64' v (GF2x64' bs) =
  U.foldl' (\x b -> min (xor x b) x) v bs == GF2x64 0

-- | /O(d)/
insertGF2x64' :: GF2x64 -> GF2x64' -> GF2x64'
insertGF2x64' v (GF2x64' bs)
  | v' == GF2x64 0 = GF2x64' bs
  | otherwise = GF2x64' (bs `U.snoc` v')
  where
    v' = U.foldl' (\x b -> min (xor x b) x) v bs

-- | /O(dN)/
spanGF2x64' :: U.Vector GF2x64 -> GF2x64'
spanGF2x64' = U.foldl' (flip insertGF2x64') zeroGF2x64'

-- | /O(d)/
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
        !v' = xor v base

-- | /O(d)/
linCombGF2x64' :: GF2x64' -> GF2x64 -> GF2x64
linCombGF2x64' (GF2x64' basis) cs =
  U.ifoldl' step 0 basis
  where
    step :: GF2x64 -> Int -> GF2x64 -> GF2x64
    step v i b
      | testBit cs i = v + b
      | otherwise = v

newtype instance U.MVector s GF2x64 = MV_GF2x64 (U.MVector s Word)
newtype instance U.Vector GF2x64 = V_GF2x64 (U.Vector Word)
deriving newtype instance GM.MVector U.MVector GF2x64
deriving newtype instance G.Vector U.Vector GF2x64
instance U.Unbox GF2x64
