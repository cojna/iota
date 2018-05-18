{-# LANGUAGE BangPatterns #-}

module Search.BinarySearch where

import           Control.Exception

lowerBound :: (Integral i) => i -> i -> (i -> Bool) -> i
lowerBound low high p = assert (p high) $ go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2
{-# INLINE lowerBound #-}

upperBound :: (Integral i) => i -> i -> (i -> Bool) -> i
upperBound low high p
    | p high = high
    | otherwise = lowerBound low high (not.p) - 1
{-# INLINE upperBound #-}

lowerBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
lowerBoundM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = do
            pmid <- p mid
            if pmid
            then go low mid
            else go (mid + 1) high
      where
        h = toInteger high
        l = toInteger low
        mid = fromIntegral $ l + div (h - l) 2

{-# INLINE lowerBoundM #-}

upperBoundM :: (Integral i, Monad m) => i -> i -> (i -> m Bool) -> m i
upperBoundM low high p = do
    phigh <- p high
    if phigh
    then return high
    else subtract 1 <$> lowerBoundM low high (fmap not.p)
{-# INLINE upperBoundM #-}

lowerBoundDouble :: Double -> Double -> (Double -> Bool) -> Double
lowerBoundDouble low high p = assert (p high) $ go 50 low high
  where
    go !n !low !high
        | n == 0    = high
        | p mid     = go (n - 1) low mid
        | otherwise = go (n - 1) mid high
      where
        mid = (low + high) * 0.5

upperBoundDouble :: Double -> Double -> (Double -> Bool) -> Double
upperBoundDouble low high p = assert (p low) $ go 50 low high
  where
    go !n !low !high
        | n == 0    = low
        | p mid     = go (n - 1) mid high
        | otherwise = go (n - 1) low mid
      where
        mid = (low + high) * 0.5
