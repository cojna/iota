{-# LANGUAGE RecordWildCards #-}

module Math.Prime.Divisor where

import Control.Monad.ST
import Data.Int
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import My.Prelude

data DivisorCache a = DivisorCache
  { offsetDC :: !(U.Vector Int)
  , bufferDC :: !(U.Vector a)
  }

buildDivisorCache :: (Integral a, U.Unbox a) => Int -> DivisorCache a
buildDivisorCache n = runST $ do
  freq <- UM.replicate (n + 1) (1 :: Int)
  UM.write freq 0 0
  flip MS.mapM_ (2 ..< n + 1) $ \d -> do
    flip MS.mapM_ (stride d (n + 1) d) $ \x -> do
      UM.unsafeModify freq (+ 1) x
  offsetDC <- U.scanl' (+) 0 <$> U.unsafeFreeze freq
  buf <- UM.unsafeNew (U.last offsetDC)
  pos <- U.thaw offsetDC
  flip MS.mapM_ (1 ..< n + 1) $ \d -> do
    flip MS.mapM_ (stride d (n + 1) d) $ \x -> do
      i <- UM.unsafeRead pos x
      UM.unsafeWrite pos x (i + 1)
      UM.unsafeWrite buf i (fromIntegral d)
  bufferDC <- U.unsafeFreeze buf
  return DivisorCache{..}
{-# SPECIALIZE buildDivisorCache :: Int -> DivisorCache Int #-}
{-# SPECIALIZE buildDivisorCache :: Int -> DivisorCache Int32 #-}

{- |
>>> dc = buildDivisorCache @Int 100
>>> divisors dc 60
[1,2,3,4,5,6,10,12,15,20,30,60]
>>> divisors dc 0
[]
>>> divisors dc 1
[1]
>>> divisors dc 2
[1,2]
>>> divisors dc 100
[1,2,4,5,10,20,25,50,100]
-}
divisors :: (U.Unbox a) => DivisorCache a -> Int -> U.Vector a
divisors DivisorCache{..} i = U.unsafeSlice o (o' - o) bufferDC
  where
    o = U.unsafeIndex offsetDC i
    o' = U.unsafeIndex offsetDC (i + 1)
{-# INLINE divisors #-}

{- |
>>> naiveDivisors 60
[1,60,2,30,3,20,4,15,5,12,6,10]
>>> naiveDivisors 0
[]
>>> naiveDivisors 1
[1]
>>> naiveDivisors 100
[1,100,2,50,4,25,5,20,10]
>>> length $ naiveDivisors 720720
240
-}
naiveDivisors :: Int -> [Int]
naiveDivisors n = go 1
  where
    go !x
      | x * x < n = case quotRem n x of
          (q, 0) -> x : q : go (x + 1)
          _ -> go (x + 1)
      | x * x == n = [x]
      | otherwise = []

numDivisors :: (U.Unbox a) => DivisorCache a -> Int -> Int
numDivisors DivisorCache{..} i = o' - o
  where
    o = U.unsafeIndex offsetDC i
    o' = U.unsafeIndex offsetDC (i + 1)
{-# INLINE numDivisors #-}
