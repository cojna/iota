{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}

module Math.Prime.Sieve where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits (Bits (clearBit, testBit, unsafeShiftR, (.&.)))
import Data.Function (fix)
import qualified Data.List as L
import Data.Primitive (
  ByteArray,
  fillByteArray,
  indexByteArray,
  newByteArray,
  readByteArray,
  unsafeFreezeByteArray,
  writeByteArray,
 )
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64, Word8)

withPrimes :: Int -> (U.Vector Int -> a) -> a
withPrimes n f = f . U.filter isP $ U.generate (n + 1) id
  where
    !(Sieve sieved) = sieve n
    isP i =
      let seg = indexByteArray @Word64 sieved (unsafeShiftR i 6)
       in testBit seg (i .&. 0x3f)

newtype Sieve = Sieve ByteArray

sieve :: Int -> Sieve
sieve n = runST $ do
  let lim = ((n + 1) + 63) `quot` 64 * 64
  isp <- newByteArray (lim * 8)
  fillByteArray isp 0 (lim * 8) 0b10101010
  seg0 <- readByteArray @Word64 isp 0
  writeByteArray @Word8 isp 0 0b10101100
  let !sqrtLim = floor . sqrt $ fromIntegral lim
  flip fix 3 $ \loop !p -> do
    seg <- readByteArray @Word64 isp (unsafeShiftR p 6)
    when (testBit seg (p .&. 0x3f)) $ do
      flip fix (p * p) $ \loop' !i -> do
        when (i < lim) $ do
          seg' <- readByteArray @Word64 isp (unsafeShiftR i 6)
          writeByteArray @Word64 isp (unsafeShiftR i 6) $
            clearBit seg' (i .&. 0x3f)
          loop' (i + 2 * p)
    when (p + 2 <= sqrtLim) $ do
      loop (p + 2)
  Sieve <$> unsafeFreezeByteArray isp
