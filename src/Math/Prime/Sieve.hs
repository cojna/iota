{-# LANGUAGE LambdaCase #-}

module Math.Prime.Sieve where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits (Bits (clearBit, testBit, unsafeShiftR, (.&.)))
import Data.Function (fix)
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
import qualified Data.Vector.Unboxed.Mutable as UM
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
  writeByteArray @Word8 isp 0 0b10101100
  let !sqrtLim = floor . sqrt @Double $ fromIntegral lim
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

buildMoebiusTable :: Int -> U.Vector Int
buildMoebiusTable n = U.create $ do
  isp <- UM.replicate (n + 1) True
  ms <- UM.replicate (n + 1) 1
  UM.write isp 0 False
  UM.write isp 1 False
  fix
    ( \outer p -> when (p <= n) $ do
        UM.unsafeRead isp p >>= \case
          False -> pure ()
          True -> do
            UM.unsafeWrite ms p (-1)
            fix
              ( \inner q -> when (q <= n) $ do
                  UM.unsafeWrite isp q False
                  if rem (quot q p) p == 0
                    then UM.unsafeWrite ms q 0
                    else UM.unsafeModify ms negate q
                  inner $ q + p
              )
              (2 * p)
        outer (p + 1)
    )
    2
  return ms
