{-# LANGUAGE BangPatterns, BinaryLiterals #-}

module Math.Prime where

import qualified Data.List as L

smallPrimes :: (Integral i) => [i]
smallPrimes = 2 : [ n | n<-[3,5..46337], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]
{-# SPECIALIZE smallPrimes :: [Int] #-}

primeFactors :: (Integral i) => i -> [i]
primeFactors n | n < 2 = []
primeFactors n = go n smallPrimes
  where
    go !n pps@(p:ps)
        | n < p * p = [n]
        | r > 0     = go n ps
        | otherwise = p : go q pps
      where
        (q, r) = quotRem n p
    go n [] = [n]
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}

isPrime :: Integral i => i -> Bool
isPrime n = [n] == primeFactors n
{-# SPECIALIZE isPrime :: Int -> Bool #-}

totient :: Int -> Int
totient n = n `quot` product ps * product (map (subtract 1) ps)
  where
    ps = map head . L.group $ primeFactors n

divisors :: Int -> [Int]
divisors n = L.sort . map product . mapM (scanl (*) 1) . L.group $ primeFactors n
