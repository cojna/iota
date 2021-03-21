{-# LANGUAGE BangPatterns #-}

module Algorithm.BinarySearch where

import Control.Monad
import Data.Bool
import Data.Functor.Identity

import My.Prelude (unsafeShiftRL)

-- | assert (p high)
lowerBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
lowerBoundM low high p = go low high
  where
    go !low !high
      | high <= low = return high
      | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid)
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE lowerBoundM #-}

-- | assert (p low)
upperBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
upperBoundM low high p = do
  flg <- p high
  if flg
    then return high
    else subtract 1 <$!> lowerBoundM low high (fmap not . p)
{-# INLINE upperBoundM #-}

-- | assert (p high)
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = runIdentity (lowerBoundM low high (return . p))
{-# INLINE lowerBound #-}

-- | assert (p low)
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p = runIdentity (upperBoundM low high (return . p))
{-# INLINE upperBound #-}

lowerBoundInteger :: Integer -> Integer -> (Integer -> Bool) -> Integer
lowerBoundInteger low high p =
  runIdentity $
    lowerBoundIntegerM low high (pure . p)
{-# INLINE lowerBoundInteger #-}

upperBoundInteger :: Integer -> Integer -> (Integer -> Bool) -> Integer
upperBoundInteger low high p =
  runIdentity $
    upperBoundIntegerM low high (pure . p)
{-# INLINE upperBoundInteger #-}

lowerBoundIntegerM ::
  (Monad m) => Integer -> Integer -> (Integer -> m Bool) -> m Integer
lowerBoundIntegerM low high p = go low high
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
{-# INLINE lowerBoundIntegerM #-}

upperBoundIntegerM ::
  (Monad m) => Integer -> Integer -> (Integer -> m Bool) -> m Integer
upperBoundIntegerM low high p = do
  phigh <- p high
  if phigh
    then return high
    else subtract 1 <$> lowerBoundIntegerM low high (fmap not . p)
{-# INLINE upperBoundIntegerM #-}

-- | assert (p high)
lowerBoundDouble :: Double -> Double -> (Double -> Bool) -> Double
lowerBoundDouble low high p = go 50 low high
  where
    go !n !low !high
      | n == 0 = high
      | p mid = go (n - 1) low mid
      | otherwise = go (n - 1) mid high
      where
        mid = (low + high) * 0.5

-- | assert (p low)
upperBoundDouble :: Double -> Double -> (Double -> Bool) -> Double
upperBoundDouble low high p = go 50 low high
  where
    go !n !low !high
      | n == 0 = low
      | p mid = go (n - 1) mid high
      | otherwise = go (n - 1) low mid
      where
        mid = (low + high) * 0.5
