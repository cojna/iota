{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.IntModBench (benchMain) where

import Criterion
import Data.GaloisField
import Data.IntMod
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import System.Random hiding (randoms)

#define MOD 998244353

benchMain :: Benchmark
benchMain =
  bgroup
    "IntMod"
    [ bgroup
        "(+%)"
        [ bench "(+%)" $ whnf (U.foldl' (+%) 0) randoms
        , bench "addModGF" $ whnf (U.foldl' addModGF 0) randoms
        , bench "addMod1" $ whnf (U.foldl' addMod1 0) randoms
        , bench "addMod2" $ whnf (U.foldl' addMod2 0) randoms
        , bench "addMod3" $ whnf (U.foldl' addMod3 0) randoms
        , bench "addMod4" $ whnf (U.foldl' addMod4 0) randoms
        , bench "addMod5" $ whnf (U.foldl' addMod5 0) randoms
        ]
    , bgroup
        "(-%)"
        [ bench "(-%)" $ whnf (U.foldl' (-%) 0) randoms
        , bench "subModGF" $ whnf (U.foldl' subModGF 0) randoms
        , bench "subMod1" $ whnf (U.foldl' subMod1 0) randoms
        , bench "subMod2" $ whnf (U.foldl' subMod2 0) randoms
        , bench "subMod3" $ whnf (U.foldl' subMod3 0) randoms
        , bench "subMod4" $ whnf (U.foldl' subMod4 0) randoms
        , bench "subMod5" $ whnf (U.foldl' subMod5 0) randoms
        , bench "subMod6" $ whnf (U.foldl' subMod6 0) randoms
        , bench "subMod7" $ whnf (U.foldl' subMod7 0) randoms
        ]
    , bgroup
        "(*%)"
        [ bench "(*%)" $ whnf (U.foldl' (*%) 1) randoms
        , bench "timesModGF" $ whnf (U.foldl' timesModGF 1) randoms
        , bench "timesMod1" $ whnf (U.foldl' timesMod1 1) randoms
        , bench "timesMod2" $ whnf (U.foldl' timesMod2 1) randoms
        , bench "timesMod3" $ whnf (U.foldl' timesMod3 1) randoms
        , bench "timesMod4" $ whnf (U.foldl' timesMod4 1) randoms
        , bench "timesMod5" $ whnf (U.foldl' timesMod5 1) randoms
        , bench "timesMod6" $ whnf (U.foldl' timesMod6 1) randoms
        , bench "timesMod7" $ whnf (U.foldl' timesMod7 1) randoms
        ]
    , bgroup
        "(/%)"
        [ bench "divModGF" $ whnf (U.foldl' divModGF 1) randoms
        , bench "divMod1" $ whnf (U.foldl' divMod1 1) randoms
        , bench "divMod2" $ whnf (U.foldl' divMod2 1) randoms
        ]
    ]
  where
    n = 10000
    randoms :: U.Vector Int
    randoms =
      U.map fromIntegral
        $ U.unfoldrExactN n (genWord64R (modulus - 1)) (mkStdGen 123456789)

addModGF :: Int -> Int -> Int
addModGF = coerce ((+) @(GF MOD))

addMod1 :: Int -> Int -> Int
addMod1 (I# x#) (I# y#) = I# ((x# +# y#) `remInt#` MOD#)

addMod2 :: Int -> Int -> Int
addMod2 (I# x#) (I# y#) = case x# +# y# of
  r#
    | isTrue# (r# <# MOD#) -> I# r#
    | otherwise -> I# (r# -# MOD#)

-- current
addMod3 :: Int -> Int -> Int
addMod3 (I# x#) (I# y#) = case x# +# y# of
  r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))

addMod4 :: Int -> Int -> Int
addMod4 (I# x#) (I# y#) = case x# +# y# of
  r# -> I# (r# -# (MOD# *# (r# >=# MOD#)))

addMod5 :: Int -> Int -> Int
addMod5 (I# x#) (I# y#) = I# (x# +# y# -# (MOD# *# (x# +# y# >=# MOD#)))

subModGF :: Int -> Int -> Int
subModGF = coerce ((-) @(GF MOD))

subMod1 :: Int -> Int -> Int
subMod1 (I# x#) (I# y#) = I# ((x# -# y# +# MOD#) `remInt#` MOD#)

subMod2 :: Int -> Int -> Int
subMod2 (I# x#) (I# y#) = case x# -# y# of
  r#
    | isTrue# (r# >=# 0#) -> I# r#
    | otherwise -> I# (r# +# MOD#)

subMod3 :: Int -> Int -> Int
subMod3 (I# x#) (I# y#)
  | isTrue# (x# >=# y#) = I# (x# -# y#)
  | otherwise = I# (x# -# y# +# MOD#)

-- current
subMod4 :: Int -> Int -> Int
subMod4 (I# x#) (I# y#) = case x# -# y# of
  r# -> I# (r# +# ((r# <# 0#) *# MOD#))

subMod5 :: Int -> Int -> Int
subMod5 (I# x#) (I# y#) = case x# -# y# of
  r# -> I# (r# +# (MOD# *# (r# <# 0#)))

subMod6 :: Int -> Int -> Int
subMod6 (I# x#) (I# y#) = I# (x# -# y# +# ((x# <# y#) *# MOD#))

subMod7 :: Int -> Int -> Int
subMod7 (I# x#) (I# y#) = I# (x# -# y# +# (MOD# *# (x# <# y#)))

#define INV_MOD 18446743945

timesModGF :: Int -> Int -> Int
timesModGF = coerce ((*) @(GF MOD))

timesMod1 :: Int -> Int -> Int
timesMod1 (I# x#) (I# y#) = I# (x# *# y# `remInt#` MOD#)

timesMod2 :: Int -> Int -> Int
timesMod2 (I# x#) (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# INV_MOD## of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# MOD##) of
      v# -> I# (word2Int# v# +# leWord# MOD## v# *# MOD#)

timesMod3 :: Int -> Int -> Int
timesMod3 (I# x#) (I# y#) = case int2Word# (x# *# y#) of
  z# -> case timesWord2# z# INV_MOD## of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# MOD##) of
      v# -> I# (word2Int# v# +# leWord# MOD## v# *# MOD#)

timesMod4 :: Int -> Int -> Int
timesMod4 (I# x#) (I# y#) = case int2Word# (x# *# y#) of
  z# -> case timesWord2# z# INV_MOD## of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# MOD##) of
      v# -> I# (geWord# v# MOD## *# MOD# +# word2Int# v#)

timesMod5 :: Int -> Int -> Int
timesMod5 (I# x#) (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# MOD##) of
      v# -> I# (word2Int# v# +# leWord# MOD## v# *# MOD#)
  where
    im# = plusWord# (quotWord# 0xffffffffffffffff## MOD##) 1##

timesMod6 :: Int -> Int -> Int
timesMod6 (I# x#) (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# MOD##) of
      v#
        | isTrue# (geWord# v# MOD##) -> I# (word2Int# (plusWord# v# MOD##))
        | otherwise -> I# (word2Int# v#)
  where
    im# = plusWord# (quotWord# 0xffffffffffffffff## MOD##) 1##

timesMod7 :: Int -> Int -> Int
timesMod7 (I# x#) (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
      v#
        | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
        | otherwise -> I# (word2Int# v#)
  where
    m# = int2Word# MOD#
    im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##

divModGF :: Int -> Int -> Int
divModGF = coerce ((/) @(GF MOD))

divMod1 :: Int -> Int -> Int
divMod1 (I# x#) (I# y#) = go# y# m# 1# 0#
  where
    !(I# m#) = MOD
    go# a# b# u# v#
      | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
          q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
      | otherwise = I# ((x# *# (u# +# m#)) `remInt#` m#)

divMod2 :: Int -> Int -> Int
divMod2 x y = coerce f
  where
    f = (GF x :: GF MOD) * (GF y ^ (MOD - 2 :: Int))
