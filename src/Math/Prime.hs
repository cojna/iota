{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module Math.Prime where

import qualified Data.IntMap as IM
import qualified Data.List as L

smallPrimes :: (Integral i) => [i]
smallPrimes = 2 : [n | n <- [3, 5 .. 46337], all ((> 0) . rem n) $ takeWhile (\x -> x * x <= n) smallPrimes]
{-# SPECIALIZE smallPrimes :: [Int] #-}

{- |
>>> primeFactors 60
[2,2,3,5]
>>> primeFactors 0
[]
>>> primeFactors 1
[]
>>> primeFactors 2
[2]
>>> primeFactors 2147483647
[2147483647]
>>> primeFactors 999999999989
[999999999989]
>>> primeFactors 999999999997
[5507,181587071]
-}
primeFactors :: (Integral i) => i -> [i]
primeFactors n | n < 2 = []
primeFactors n0 = go n0 smallPrimes
  where
    go !n pps@(p : ps)
      | n < p * p = [n]
      | r > 0 = go n ps
      | otherwise = p : go q pps
      where
        (q, r) = quotRem n p
    go n [] = go n [46339, 46341 ..]
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}

{- |
>>> isPrime 4649
True
>>> isPrime 0
False
>>> isPrime 1
False
>>> isPrime 2
True
>>> isPrime 2147483647
True
>>> isPrime 999999999989
True
>>> isPrime 999999999997
False
-}
isPrime :: Integral i => i -> Bool
isPrime n = [n] == primeFactors n
{-# SPECIALIZE isPrime :: Int -> Bool #-}

{- |
prop> \n -> not (n >= 0) || totient n == length [x|x<-[1..n], gcd n x == 1]
+++ OK, passed 100 tests.

>>> totient 0
0
>>> totient 1
1
-}
totient :: Int -> Int
totient n = n `quot` product ps * product (map (subtract 1) ps)
  where
    ps = map head . L.group $ primeFactors n

{- |
>>> divisors 12
[1,2,3,4,6,12]
>>> divisors 0
[1]
>>> divisors 1
[1]
>>> length (divisors 735134400)
1344
-}
divisors :: Int -> [Int]
divisors n = L.sort . map product . mapM (scanl (*) 1) . L.group $ primeFactors n

{- |
>>> moebius 1
1
>>> moebius 2
-1
>>> moebius 3
-1
>>> moebius (2 * 2)
0
>>> moebius (2 * 3)
1
>>> moebius (2 * 2 * 3)
0
-}
moebius :: Int -> Int
moebius n = product . map f . L.group $ primeFactors n
  where
    f [_] = -1
    f _ = 0

{- |
>>> moebiusInversion 12 (length.divisors)
fromList [(1,1),(2,1),(3,1),(4,1),(6,1),(12,1)]
>>> moebiusInversion 999999999997 (length.divisors)
fromList [(1,1),(5507,1),(181587071,1),(999999999997,1)]
>>> IM.size $ moebiusInversion 735134400 (length.divisors)
1344
-}
moebiusInversion :: (Num a) => Int -> (Int -> a) -> IM.IntMap a
moebiusInversion n f = L.foldl' step (IM.fromAscList [(d, f d) | d <- ds]) ds
  where
    !ds = divisors n
    step g d =
      let !gd = g IM.! d
       in IM.mapWithKey
            ( \k v ->
                if k > d && rem k d == 0 then v - gd else v
            )
            g
{-# INLINE moebiusInversion #-}
