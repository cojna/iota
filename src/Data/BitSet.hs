{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Data.BitSet where

import Data.Bits
import Data.Coerce
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Exts

{- $setup
>>> :set -XOverloadedLists
-}

newtype BitSet = BitSet {getBitSet :: Int}
  deriving (Eq, Ord)
  deriving newtype (Num, Bits, FiniteBits)

instance Show BitSet where
  showsPrec p xs =
    showParen (p > 10) $
      showString "fromList " . shows (toList xs)

instance IsList BitSet where
  type Item BitSet = Int
  fromList = BitSet . F.foldl' (\acc x -> acc .|. unsafeShiftL 1 x) 0
  toList = toListBS

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
>>> isSubsetOfBS [] [1,2,3]
True
>>> isSubsetOfBS [1] []
False
>>> isSubsetOfBS [1,2,3] [1,2,3]
True
-}
isSubsetOfBS :: BitSet -> BitSet -> Bool
isSubsetOfBS x y = intersectionBS x y == x

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

deleteFindMinBS :: BitSet -> (Int, BitSet)
deleteFindMinBS x = (findMinBS x, deleteMinBS x)

deleteFindMaxBS :: BitSet -> (Int, BitSet)
deleteFindMaxBS x =
  let i = findMaxBS x
   in (i, deleteBS i x)

minViewBS :: BitSet -> Maybe (Int, BitSet)
minViewBS x
  | x /= BitSet 0 = Just $ deleteFindMinBS x
  | otherwise = Nothing

maxViewBS :: BitSet -> Maybe (Int, BitSet)
maxViewBS x
  | x /= BitSet 0 = Just $ deleteFindMaxBS x
  | otherwise = Nothing

{- |
>>> import Data.Functor.Identity
>>> runIdentity . MS.toList $ powersetBS [0,1,2]
[fromList [0,1,2],fromList [1,2],fromList [0,2],fromList [2],fromList [0,1],fromList [1],fromList [0],fromList []]
>>> runIdentity . MS.toList $ powersetBS []
[fromList []]
-}
powersetBS :: (Monad m) => BitSet -> MS.Stream m BitSet
powersetBS s0 = MS.Stream step (s0 + 1)
  where
    step s
      | s' < s = return $ MS.Yield s' s'
      | otherwise = return MS.Done
      where
        !s' = (s - 1) .&. s0
    {-# INLINE [0] step #-}
{-# INLINE [1] powersetBS #-}

{- |
>>> import Data.Functor.Identity
>>> runIdentity . MS.toList $ strictPowersetBS [0,1,2]
[fromList [1,2],fromList [0,2],fromList [2],fromList [0,1],fromList [1],fromList [0],fromList []]
>>> runIdentity . MS.toList $ strictPowersetBS []
[]
-}
strictPowersetBS :: (Monad m) => BitSet -> MS.Stream m BitSet
strictPowersetBS s0 = MS.Stream step s0
  where
    step s
      | s' < s = return $ MS.Yield s' s'
      | otherwise = return MS.Done
      where
        !s' = (s - 1) .&. s0
    {-# INLINE [0] step #-}
{-# INLINE [1] strictPowersetBS #-}

{- |
>>> toListBS [0,1,63]
[0,1,63]
>>> toListBS [0]
[0]
>>> toListBS []
[]
-}
toListBS :: BitSet -> [Int]
toListBS = L.unfoldr minViewBS

{- |
>>> import Data.Functor.Identity
>>> runIdentity . MS.toList $ toStreamBS [3,1,4]
[1,3,4]
-}
toStreamBS :: (Monad m) => BitSet -> MS.Stream m Int
toStreamBS = MS.Stream step
  where
    step s
      | s /= BitSet 0 = return $ MS.Yield (findMinBS s) (deleteMinBS s)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] toStreamBS #-}

newtype instance U.MVector s BitSet = MV_BitSet (U.MVector s Int)
newtype instance U.Vector BitSet = V_BitSet (U.Vector Int)
deriving newtype instance GM.MVector U.MVector BitSet
deriving newtype instance G.Vector U.Vector BitSet
instance U.Unbox BitSet
