{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.MultisetHash where

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import System.Random

newtype MultisetHash = MultisetHash Int
    deriving (Show)
    deriving newtype (Eq, Ord, Num, Random)
    deriving (Semigroup, Monoid) via (Sum Int)

newtype instance U.MVector s MultisetHash = MV_MultisetHash (U.MVector s Int)
newtype instance U.Vector MultisetHash = V_MultisetHash (U.Vector Int)
deriving newtype instance GM.MVector U.MVector MultisetHash
deriving newtype instance G.Vector U.Vector MultisetHash
instance U.Unbox MultisetHash

nullMH :: MultisetHash -> Bool
nullMH = (== mempty)

emptyMH :: MultisetHash
emptyMH = mempty

singletonMH :: (a -> MultisetHash) -> a -> MultisetHash
singletonMH = id

{- |
>>> hs = randoms (mkStdGen 123)
>>> fromListMH (hs!!) [1,2]
MultisetHash (-6020088438187520526)
>>> fromListMH (hs!!) [1,1,2]
MultisetHash (-9031158793612401624)
>>> insertMH (hs!!) 1 (fromListMH (hs!!) [1,2])
MultisetHash (-9031158793612401624)
>>> hash x = hs !! mod x 10
prop> \x xs -> fromListMH hash (x:xs) == insertMH hash x (fromListMH hash xs)
+++ OK, passed 100 tests.
-}
insertMH :: (a -> MultisetHash) -> a -> MultisetHash -> MultisetHash
insertMH hash x h = hash x + h
{-# INLINE insertMH #-}

{- |
>>> hs = randoms (mkStdGen 123)
>>> fromListMH (hs!!) [1,2]
MultisetHash (-6020088438187520526)
>>> fromListMH (hs!!) [1,1,2]
MultisetHash (-9031158793612401624)
>>> deleteMH (hs!!) 1 (fromListMH (hs!!) [1,1,2])
MultisetHash (-6020088438187520526)
-}
deleteMH :: (a -> MultisetHash) -> a -> MultisetHash -> MultisetHash
deleteMH hash x h = h - hash x
{-# INLINE deleteMH #-}

{- |
>>> hs = randoms (mkStdGen 123)
>>> unionMH (fromListMH (hs!!) [1,1,2]) (fromListMH (hs!!) [1,3])
MultisetHash 6625208473658164035
>>> fromListMH (hs!!) $ [1,1,2] <> [1,3]
MultisetHash 6625208473658164035
-}
unionMH :: MultisetHash -> MultisetHash -> MultisetHash
unionMH = (+)
{-# INLINE unionMH #-}

{- |
>>> hs = randoms (mkStdGen 123)
>>> differenceMH (fromListMH (hs!!) [1,1,2]) (fromListMH (hs!!) [1,2])
MultisetHash (-3011070355424881098)
>>> fromListMH (hs!!) [1]
MultisetHash (-3011070355424881098)
>>> differenceMH (fromListMH (hs!!) [1,1,2]) (fromListMH (hs!!) [1,3])
MultisetHash (-6240781987173415667)
-}
differenceMH :: MultisetHash -> MultisetHash -> MultisetHash
differenceMH = (-)
{-# INLINE differenceMH #-}

{- |
>>> hs = randoms (mkStdGen 123)
>>> fromListMH (hs !!) [1,1,1,2,2,3]
MultisetHash 3616190390895524607
>>> fromListMH (hs !!) [1,2,1,3,1,2]
MultisetHash 3616190390895524607
>>> map (fromListMH (hs!!)) [[1],[1,1],[1,1,1]]
[MultisetHash (-3011070355424881098),MultisetHash (-6022140710849762196),MultisetHash (-9033211066274643294)]
>>> hash x = hs !! mod x 10
prop> \xs ys -> fromListMH hash (xs ++ ys) == fromListMH hash xs <> fromListMH hash ys
+++ OK, passed 100 tests.
-}
fromListMH :: (a -> MultisetHash) -> [a] -> MultisetHash
fromListMH = F.foldMap'
{-# INLINE fromListMH #-}
