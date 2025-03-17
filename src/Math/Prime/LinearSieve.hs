module Math.Prime.LinearSieve where

import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import My.Prelude

data LinearSieve = LinearSieve
  { leastPrimeFactor :: !(U.Vector Int)
  , primes :: !(U.Vector Int)
  }

{- | /O(N)/

>>> primes $ buildLinearSieve 32
[2,3,5,7,11,13,17,19,23,29,31]
-}
buildLinearSieve :: Int -> LinearSieve
buildLinearSieve n = runST $ do
  lpf <- UM.replicate (n + 1) 0
  ps <- UM.replicate (primeCountUpperBound n) 0
  cntP <-
    MS.foldM'
      ( \cnt x -> do
          lpfx <- UM.unsafeRead lpf x
          let !cnt'
                | lpfx > 0 = cnt
                | otherwise = cnt + 1
          when (cnt < cnt') $ do
            UM.unsafeWrite lpf x x
            UM.unsafeWrite ps cnt x
          lpfx' <- UM.unsafeRead lpf x
          fix
            ( \loop !i -> when (i < cnt') $ do
                p <- UM.unsafeRead ps i
                when (p <= lpfx' && p * x <= n) $ do
                  UM.unsafeWrite lpf (p * x) p
                  loop (i + 1)
            )
            0
          pure cnt'
      )
      0
      (2 ..< n + 1)
  LinearSieve
    <$> U.unsafeFreeze lpf
    <*> U.unsafeFreeze (UM.take cntP ps)

{- |
>>> ls = buildLinearSieve 100
>>> primeFactors ls 60
[2,2,3,5]
>>> primeFactors ls 0
[]
>>> primeFactors ls 1
[]
>>> primeFactors ls 2
[2]
>>> primeFactors ls 4
[2,2]
-}
primeFactors :: LinearSieve -> Int -> [Int]
primeFactors LinearSieve{leastPrimeFactor = lpf} = go
  where
    go n
      | n < 2 = []
      | p <- U.unsafeIndex lpf n = p : go (quot n p)

{- |
>>> ls = buildLinearSieve 100
>>> isPrime ls 97
True
>>> isPrime ls 100
False
>>> isPrime ls 0
False
>>> isPrime ls 1
False
>>> isPrime ls 2
True
-}
isPrime :: LinearSieve -> Int -> Bool
isPrime LinearSieve{leastPrimeFactor = lpf} n =
  n >= 2 && U.unsafeIndex lpf n == n

{- |
>>> primeCountUpperBound 100
32
>>> primeCountUpperBound (10 ^ 6)
100000
-}
primeCountUpperBound :: Int -> Int
primeCountUpperBound n
  | n >= 64720 = quot n 10
  | n >= 24300 = quot n 9
  | n >= 8472 = quot n 8
  | n >= 3094 = quot n 7
  | n >= 1134 = quot n 6
  | n >= 360 = quot n 5
  | n >= 120 = quot n 4
  | otherwise = 32
