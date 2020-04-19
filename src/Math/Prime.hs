{-# LANGUAGE BangPatterns, BinaryLiterals, TypeApplications #-}


module Math.Prime where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import qualified Data.List           as L
import           Data.Primitive
import qualified Data.Vector.Unboxed as U
import           Data.Word

smallPrimes :: [Int]
smallPrimes = 2 : [ n | n<-[3,5..46337], all ((>0).rem n) $ takeWhile (\x->x*x<=n) smallPrimes]

primeFactors :: Int -> [Int]
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

isPrime :: Int -> Bool
isPrime n = [n] == primeFactors n

totient :: Int -> Int
totient n = n `quot` product ps * product (map (subtract 1) ps)
  where
    ps = map head . L.group $ primeFactors n

divisors :: Int -> [Int]
divisors n = L.sort . map product . mapM (scanl (*) 1) . L.group $ primeFactors n

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
    writeByteArray @Word8 isp 0 $ 0b10101100
    let !sqrtLim = floor . sqrt $ fromIntegral lim
    flip fix 3 $ \loop !p -> do
        seg <- readByteArray @Word64 isp (unsafeShiftR p 6)
        when (testBit seg (p .&. 0x3f)) $ do
            flip fix (p * p) $ \loop' !i -> do
                when (i < lim) $ do
                    seg' <- readByteArray @Word64 isp (unsafeShiftR i 6)
                    writeByteArray @Word64 isp (unsafeShiftR i 6)
                        $ clearBit seg' (i .&. 0x3f)
                    loop' (i + 2 * p)
        when (p + 2 <= sqrtLim) $ do
            loop (p + 2)
    Sieve <$> unsafeFreezeByteArray isp
