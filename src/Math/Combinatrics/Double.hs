{-# LANGUAGE CPP #-}

module Math.Combinatrics.Double where

import qualified Data.Vector.Unboxed as U

#define LOG_FACT_CACHE_SIZE 1024

logFactCache :: U.Vector Double
logFactCache =
  U.scanl' (+) 0.0 $
    U.generate LOG_FACT_CACHE_SIZE (log . fromIntegral . (+ 1))
{-# NOINLINE logFactCache #-}

logFact :: Int -> Double
logFact = U.unsafeIndex logFactCache
{-# INLINE logFact #-}

logPerm :: Int -> Int -> Double
logPerm n k = logFact n - logFact (n - k)
{-# INLINE logPerm #-}

logComb :: Int -> Int -> Double
logComb n k = logFact n - logFact k - logFact (n - k)
{-# INLINE logComb #-}
