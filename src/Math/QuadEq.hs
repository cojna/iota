module Math.QuadEq where

import Control.Monad
import Math.Utils

{- |

\(ax^2+bx+c=0, a\neq 0\)

>>> solveQuadEq 1 (-3) 2
[1.0,2.0]
>>> solveQuadEq 1 (-2) 1
[1.0]
>>> solveQuadEq 1 0 0
[-0.0]
>>> solveQuadEq 1 0 (-2)
[-1.4142135623730951,1.414213562373095]
>>> solveQuadEq 1 (-1) (-1)
[-0.6180339887498948,1.618033988749895]
>>> solveQuadEq 1 (-2147483648) 2147483647
[1.0,2.147483647e9]
>>> solveQuadEq 1 (-2147483648) 1
[4.656612873077393e-10,2.147483648e9]
>>> solveQuadEq 0 1 1
*** Exception: solveQuadEq: (0,1,1)
-}
solveQuadEq :: Integer -> Integer -> Integer -> [Double]
solveQuadEq a b c
  | a == 0 = error $ "solveQuadEq: " <> show (a, b, c)
  | d < 0 = []
  | d == 0 = [-b' / (2 * a')]
  | b < 0
  , x0 <- 2 * c' / (-b' + sqrt d')
  , x1 <- (-b' + sqrt d') / (2 * a') =
      if a > 0 then [x0, x1] else [x1, x0]
  | x0 <- (-b' - sqrt d') / (2 * a')
  , x1 <- 2 * c' / (-b' - sqrt d') =
      if a > 0 then [x0, x1] else [x1, x0]
  where
    d = b * b - 4 * a * c
    a' = fromIntegral a
    b' = fromIntegral b
    c' = fromIntegral c
    d' = fromIntegral d

{- |

\(ax^2+bx+c=0, a\neq 0\)

>>> solveQuadEqInteger 1 (-3) 2
[1,2]
>>> solveQuadEqInteger 1 (-2) 1
[1]
>>> solveQuadEqInteger 1 0 0
[0]
>>> solveQuadEqInteger 1 0 (-2)
[]
>>> solveQuadEqInteger 1 (-1) (-1)
[]
>>> solveQuadEqInteger 0 1 1
*** Exception: solveQuadEqInteger: (0,1,1)
-}
solveQuadEqInteger :: Integer -> Integer -> Integer -> [Integer]
solveQuadEqInteger a b c
  | a == 0 = error $ "solveQuadEqInteger: " <> show (a, b, c)
  | a < 0 = solveQuadEqInteger (-a) (-b) (-c)
solveQuadEqInteger a b c
  | d < 0 = []
  | d == 0, (x, 0) <- quotRem (-b) (2 * a) = [x]
  | otherwise = do
      let !sqrtD = integerFloorSqrt d
      guard $ sqrtD * sqrtD == d
      (!x, 0) <- map (`quotRem` (2 * a)) [-b - sqrtD, -b + sqrtD]
      pure x
  where
    !d = b * b - 4 * a * c
