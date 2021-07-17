{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.BitSet where

import Control.Monad
import Data.Bits
import Data.Coerce
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts

{- $setup
>>> :set -XOverloadedLists
-}

newtype BitSet = BitSet {getBitSet :: Int} deriving (Eq, Ord)

instance Show BitSet where
  showsPrec p xs =
    showParen (p > 10) $
      showString "fromList " . shows (toList xs)

instance IsList BitSet where
  type Item BitSet = Int
  fromList = BitSet . F.foldl' (\acc x -> acc .|. unsafeShiftL 1 x) 0
  toList bs = filter (`memberBS` bs) [0 .. 63]

emptyBS :: BitSet
emptyBS = BitSet 0

singletonBS :: Int -> BitSet
singletonBS (I# i#) = BitSet (I# (uncheckedIShiftL# 1# i#))

{- |
>>> insertBS 0 []
fromList [0]
>>> insertBS 0 [0]
fromList [0]
-}
insertBS :: Int -> BitSet -> BitSet
insertBS (I# i#) (BitSet (I# bs#)) =
  BitSet (I# (uncheckedIShiftL# 1# i# `orI#` bs#))

{- |
>>> deleteBS 2 [1,2,3]
fromList [1,3]
>>> deleteBS 10 [1,2,3]
fromList [1,2,3]
>>> deleteBS 0 []
fromList []
-}
deleteBS :: Int -> BitSet -> BitSet
deleteBS (I# i#) (BitSet (I# bs#)) =
  BitSet (I# (notI# (uncheckedIShiftL# 1# i#) `andI#` bs#))

memberBS :: Int -> BitSet -> Bool
memberBS (I# i#) (BitSet (I# bs#)) =
  isTrue# (uncheckedIShiftRL# bs# i# `andI#` 1#)

notMemberBS :: Int -> BitSet -> Bool
notMemberBS i = not . memberBS i

nullBS :: BitSet -> Bool
nullBS = (== 0) . coerce @BitSet @Int

sizeBS :: BitSet -> Int
sizeBS = coerce (popCount @Int)

{- |
>>> isSubsetOf [] [1,2,3]
True
>>> isSubsetOf [1] []
False
>>> isSubsetOf [1,2,3] [1,2,3]
True
-}
isSubsetOf :: BitSet -> BitSet -> Bool
isSubsetOf x y = intersectionBS x y == x

unionBS :: BitSet -> BitSet -> BitSet
unionBS = coerce ((.|.) @Int)

complementBS :: BitSet -> BitSet
complementBS = coerce (complement @Int)

{-
>>> differenceBS [1,2,3] [1,2]
fromList [3]
>>> differenceBS [1,2] [1,2,3]
fromList []
-}
differenceBS :: BitSet -> BitSet -> BitSet
differenceBS x y = intersectionBS x (complementBS y)

intersectionBS :: BitSet -> BitSet -> BitSet
intersectionBS = coerce ((.&.) @Int)

{- |
>>> findMinBS [1,2,3]
1
>>> findMinBS []
64
-}
findMinBS :: BitSet -> Int
findMinBS = coerce (countTrailingZeros @Int)

{- |
>>> findMaxBS [1,2,3]
3
>>> findMaxBS []
-1
-}
findMaxBS :: BitSet -> Int
findMaxBS = (63 -) . coerce (countLeadingZeros @Int)

{- |
>>> deleteMinBS [1,2,3]
fromList [2,3]
>>> deleteMinBS []
fromList []
-}
deleteMinBS :: BitSet -> BitSet
deleteMinBS (BitSet x) = BitSet (x .&. (x - 1))

{- |
>>> deleteMaxBS [1,2,3]
fromList [1,2]
>>> deleteMaxBS []
fromList []
-}
deleteMaxBS :: BitSet -> BitSet
deleteMaxBS x = deleteBS (findMaxBS x) x

deleteFindMin :: BitSet -> (Int, BitSet)
deleteFindMin x = (findMinBS x, deleteMinBS x)

deleteFindMax :: BitSet -> (Int, BitSet)
deleteFindMax x =
  let i = findMaxBS x
   in (i, deleteBS i x)

minView :: BitSet -> Maybe (Int, BitSet)
minView x
  | x /= BitSet 0 = Just $ deleteFindMin x
  | otherwise = Nothing

maxView :: BitSet -> Maybe (Int, BitSet)
maxView x
  | x /= BitSet 0 = Just $ deleteFindMax x
  | otherwise = Nothing

newtype instance UM.MVector s BitSet = MV_BitSet (UM.MVector s Int)
newtype instance U.Vector BitSet = V_BitSet (U.Vector Int)

instance U.Unbox BitSet

instance GM.MVector UM.MVector BitSet where
  basicLength (MV_BitSet v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_BitSet v) = MV_BitSet $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_BitSet v1) (MV_BitSet v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_BitSet `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_BitSet v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_BitSet `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_BitSet v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_BitSet v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_BitSet v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_BitSet v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_BitSet v1) (MV_BitSet v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_BitSet v1) (MV_BitSet v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_BitSet v) n = MV_BitSet `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance G.Vector U.Vector BitSet where
  basicUnsafeFreeze (MV_BitSet v) = V_BitSet `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_BitSet v) = MV_BitSet `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_BitSet v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_BitSet v) = V_BitSet $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_BitSet v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_BitSet mv) (V_BitSet v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
