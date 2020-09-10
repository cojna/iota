{-# LANGUAGE BangPatterns #-}

module Math.NTT where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Bits.Utils             (bitReverse)
import           Math.Modulus                (powMod, recipMod)
import           Utils                       (unsafeShiftRL)

-- | Number Theoretic Transform
--
-- /O(n log n)/
--
-- >>> ntt 998244353 3 [1,1,1,1]
-- [4,0,0,0]
-- >>> ntt 469762049 3 [123,0,0,0]
-- [123,123,123,123]
ntt
    :: Int          -- ^ prime (c * 2 ^ k + 1)
    -> Int          -- ^ primitive root
    -> U.Vector Int -- ^ n = 2 ^ i, n < 2 ^ k
    -> U.Vector Int
ntt p g f = runST $ do
    ff <- U.unsafeThaw $ U.unsafeBackpermute f
        $ U.generate n ((`unsafeShiftRL` (64 - logN)) . bitReverse)
    U.forM_ (U.iterateN logN (*2) 2) $ \m -> do
        let !unity = powMod g (quot (p - 1) m) p -- rem (p - 1) m == 0
        let !unities = U.iterateN (unsafeShiftRL m 1) ((`rem` p) . (* unity)) 1
        fix (\loop !k -> when (k < n) $ do
            flip U.imapM_ unities $ \j w -> do
                u <- UM.unsafeRead ff (k + j)
                t <- (* w) <$!> UM.unsafeRead ff (k + j + unsafeShiftRL m 1)
                UM.unsafeWrite ff (k + j) $ rem (u + t) p
                UM.unsafeWrite ff (k + j + unsafeShiftRL m 1) $ mod (u - t) p
            loop (k + m)
            ) 0
    U.unsafeFreeze ff
  where
    !n = U.length f
    !logN = countTrailingZeros n
{-# INLINE ntt #-}

intt :: Int -> Int -> U.Vector Int -> U.Vector Int
intt p g f = U.map ((`rem` p) . (* n')) $ ntt p (recipMod g p) f
  where
    !n' = recipMod (U.length f) p
{-# INLINE intt #-}

-- |
-- >>> convolute 998244353 3 [1,1,1,0] [1,1,1,0]
-- [1,2,3,2,1,0,0]
-- >>> convolute 998244353 3 [1,1,1] [1,1,1,0]
-- [1,2,3,2,1,0]
convolute :: Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
convolute p g xs ys
    = U.take l
    . intt p g
    $ U.zipWith (\x y -> x * y `rem` p)
        (ntt p g $ xs U.++ U.replicate (extendToPowerOfTwo l - n) 0)
        (ntt p g $ ys U.++ U.replicate (extendToPowerOfTwo l - m) 0)
  where
    !n = U.length xs
    !m = U.length ys
    !l = n + m - 1

-- |
-- >>> growToPowerOfTwo [1,2,3]
-- [1,2,3,0]
growToPowerOfTwo :: U.Vector Int -> U.Vector Int
growToPowerOfTwo v
    | U.null v = U.singleton 0
    | U.length v == 1 = v
    | n <- unsafeShiftRL (-1) (countLeadingZeros (U.length v - 1)) + 1
        = v U.++ U.replicate (n - U.length v) 0

-- |
-- >>> extendToPowerOfTwo 0
-- 1
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1
