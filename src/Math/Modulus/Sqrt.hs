{-# LANGUAGE BangPatterns #-}

module Math.Modulus.Sqrt where

import Data.Bits (Bits (unsafeShiftR, (.&.)))
import Data.Function (fix)
import Math.Modulus (powMod)
import System.Random.XoRoShiRo (nextInt, withRNG)

{- | Legendre symbol (Euler's criterion)

 p is /odd/ prime
-}
legendreSymbol :: Int -> Int -> Int
legendreSymbol a p = powMod a (quot (p - 1) 2) p

{- |
 @x^2 = a (mod p)@ p is prime

 >>> sqrtMod 2 1000000007
 [59713600,940286407]
 >>> sqrtMod 3 1000000007
 [82062379,917937628]
 >>> sqrtMod 4 1000000007
 [2,1000000005]
 >>> sqrtMod 5 1000000007
 []
-}
sqrtMod :: Int -> Int -> [Int]
sqrtMod 0 _ = [0]
sqrtMod a 2 = [mod a 2]
sqrtMod a p = case legendreSymbol a p of
  0 -> [0]
  1
    | rem p 4 == 3 -> withConjugate $ powMod a (quot (p + 1) 4) p
    | rem p 4 == 1 -> withConjugate $ cipolla (mod a p) p
  x | x == p - 1 -> []
  _ -> error $ "sqrtMod: unreachable (a, p) = " <> show (a, p)
  where
    withConjugate !x =
      let !x' = p - x
       in [min x x', max x x']

{- |
 @cipolla a p ^ 2 = a (mod p)@ p is odd prime, (a | p) = 1

 >>> cipolla 9 998244353
 3
 >>> cipolla 9 1000000007
 1000000004
-}
cipolla :: Int -> Int -> Int
cipolla a p = fst $ pow (ns, 1) (quot (p + 1) 2)
  where
    !ns = withRNG $ \rng -> do
      fix $ \loop -> do
        !x <- flip mod p <$> nextInt rng
        case legendreSymbol (x *% x -% a) p of
          0 -> loop
          1 -> loop
          _ -> pure x
    !ww = ns *% ns -% a

    mul (x0, y0) (x1, y1) = (x', y')
      where
        !x' = x0 *% x1 +% y0 *% y1 *% ww
        !y' = x0 *% y1 +% y0 *% x1

    pow _ 0 = (1, 0)
    pow xy n = go (1, 0) xy n
      where
        go !acc !xy !i
          | i .&. 1 == 0 = go acc (mul xy xy) (unsafeShiftR i 1)
          | i == 1 = mul acc xy
          | otherwise = go (mul acc xy) (mul xy xy) (unsafeShiftR (i - 1) 1)

    infixl 7 *%
    infixl 6 +%, -%

    x +% y = case x + y of
      r
        | r < p -> r
        | otherwise -> r - p
    {-# INLINE (+%) #-}

    x -% y = case x - y of
      r
        | r < 0 -> r + p
        | otherwise -> r
    {-# INLINE (-%) #-}

    x *% y = x * y `rem` p
    {-# INLINE (*%) #-}
