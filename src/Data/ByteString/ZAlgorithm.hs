module Data.ByteString.ZAlgorithm where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Function (fix)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |

@z[i] = lcp s $ drop i s@

time complexity: /O(n)/

>>> :set -XOverloadedStrings
>>> zAlgorithm "ababab"
[6,0,4,0,2,0]
>>> zAlgorithm "abc$xabcxx"
[10,0,0,0,0,3,0,0,0,0]
>>> zAlgorithm ""
[]
-}
zAlgorithm :: B.ByteString -> U.Vector Int
zAlgorithm bs = U.create $ do
  z <- UM.unsafeNew n
  fix
    ( \loop !zl !zr !l -> when (l < n) $ do
        if zr <= l
          then do
            let !r = lcp 0 l
            UM.unsafeWrite z l (r - l)
            loop l r (l + 1)
          else do
            zk <- UM.unsafeRead z (l - zl)
            if zk < zr - l
              then do
                UM.unsafeWrite z l zk
                loop zl zr (l + 1)
              else do
                let !r = lcp (zr - l) zr
                UM.unsafeWrite z l (r - l)
                loop l r (l + 1)
    )
    0
    1
    1
  when (n > 0) $ do
    UM.write z 0 n
  return z
  where
    !n = B.length bs

    lcp :: Int -> Int -> Int
    lcp = fix $ \loop !l !r ->
      if r < n && B.unsafeIndex bs l == B.unsafeIndex bs r
        then loop (l + 1) (r + 1)
        else r
