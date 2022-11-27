{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Manacher where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Function (fix)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |
/n/ must be odd

/O(n)/

>>> import qualified Data.ByteString.Char8 as C
>>> manacher (C.pack "a")
[1]
>>> manacher (C.pack "aba")
[1,2,1]
>>> manacher (C.pack "a$b$b$a")
[1,1,2,4,2,1,1]
-}
manacher :: B.ByteString -> U.Vector Int
manacher bs = assert (odd n) $
  U.create $ do
    rad <- UM.replicate n 0
    fix `flip` 0 `flip` 0 $ \loop !center !radius -> do
      when (center < n) $ do
        let !radius' = naiveRadius center radius
        UM.unsafeWrite rad center radius'
        flip fix 1 $ \inner !r -> do
          if center - r >= 0 && center + r < n
            then do
              radL <- UM.unsafeRead rad (center - r)
              if r + radL < radius'
                then do
                  UM.unsafeWrite rad (center + r) radL
                  inner (r + 1)
                else loop (center + r) (radius' - r)
            else loop (center + r) (radius' - r)
    return rad
  where
    !n = B.length bs
    naiveRadius :: Int -> Int -> Int
    naiveRadius c r = go r
      where
        go !i
          | c - i >= 0
          , c + i < n
          , B.unsafeIndex bs (c - i) == B.unsafeIndex bs (c + i) =
              go (i + 1)
          | otherwise = i
