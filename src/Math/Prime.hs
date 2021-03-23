{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module Math.Prime where

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
