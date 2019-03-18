{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.IntMod.OperatorBench (benchMain) where

import           Criterion
import           Data.IntMod.Operator
import qualified Data.Vector.Unboxed  as U
import           GHC.Exts
import           Utils.Random

benchMain :: Benchmark
benchMain = bgroup "IntMod.Operator"
    [ bgroup "(+%)"
        [ bench "(+%)" $ whnf (U.foldl' (+%) 0) randoms
        , bench "addMod1" $ whnf (U.foldl' addMod1 0) randoms
        , bench "addMod2" $ whnf (U.foldl' addMod2 0) randoms
        , bench "addMod3" $ whnf (U.foldl' addMod3 0) randoms
        , bench "addMod4" $ whnf (U.foldl' addMod4 0) randoms
        , bench "addMod5" $ whnf (U.foldl' addMod5 0) randoms
        ]
    , bgroup "(-%)"
        [ bench "(-%)" $ whnf (U.foldl' (-%) 0) randoms
        , bench "subMod1" $ whnf (U.foldl' subMod1 0) randoms
        , bench "subMod2" $ whnf (U.foldl' subMod2 0) randoms
        , bench "subMod3" $ whnf (U.foldl' subMod3 0) randoms
        , bench "subMod4" $ whnf (U.foldl' subMod4 0) randoms
        , bench "subMod5" $ whnf (U.foldl' subMod5 0) randoms
        , bench "subMod6" $ whnf (U.foldl' subMod6 0) randoms
        , bench "subMod7" $ whnf (U.foldl' subMod7 0) randoms
        ]
    ]
  where
    n = 10000
    randoms :: U.Vector Int
    randoms = U.fromList . map (intMod.fromIntegral) $ take n xorshift128


#define MOD 1000000007

addMod1 :: Int -> Int -> Int
addMod1 (I# x#) (I# y#) = I# ((x# +# y#) `remInt#` MOD#)

addMod2 :: Int -> Int -> Int
addMod2 (I# x#) (I# y#) = case x# +# y# of
    r# | isTrue# (r# <# MOD#) -> I# r#
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

subMod1 :: Int -> Int -> Int
subMod1 (I# x#) (I# y#) = I# ((x# -# y# +# MOD#) `remInt#` MOD#)

subMod2 :: Int -> Int -> Int
subMod2 (I# x#) (I# y#) = case x# -# y# of
    r# | isTrue# (r# >=# 0#) -> I# r#
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

