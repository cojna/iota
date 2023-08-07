module Data.BSRBench (benchMain) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Bits
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import Math.Utils (floorLog2)
import Unsafe.Coerce (unsafeCoerce)

benchMain :: Benchmark
benchMain =
  bgroup
    "BSR(Bit Scan Reverse)"
    [ bgroup
        "123456789"
        [ bench "floorLog2" $ whnf floorLog2 123456789
        , bench "bsr0" $ whnf bsr0 123456789
        , bench "bsr1" $ whnf bsr1 123456789
        , bench "bsr2" $ whnf bsr2 123456789
        , bench "bsr3" $ whnf bsr3 123456789
        , bench "bsr4" $ whnf bsr4 123456789
        , bench "floorLog2" $ whnf floorLog2 123456789
        ]
    ]

bsr0 :: Int -> Int
bsr0 x =
  fromIntegral $
    unsafeShiftR (castDoubleToWord64 (fromIntegral x)) 52 - 1023

bsr1 :: Int -> Int
bsr1 x =
  fromIntegral $
    unsafeShiftR (unsafeCoerce @Double @Word64 (fromIntegral x)) 52 - 1023

bsr2 :: Int -> Int
bsr2 x = 63 - countLeadingZeros x

bsr3 :: Int -> Int
bsr3 x = xor (countLeadingZeros x) 0x3f

bsr4 :: Int -> Int
bsr4 x = xor 0x3f (countLeadingZeros x)
