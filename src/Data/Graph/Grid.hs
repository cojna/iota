module Data.Graph.Grid where

import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as U

inGrid :: Int -> Int -> Int -> Int -> Bool
inGrid h w x y = 0 <= x && x < h && 0 <= y && y < w
{-# INLINE inGrid #-}

neighbor4 :: (Applicative f) => Int -> Int -> (Int -> Int -> f ()) -> f ()
neighbor4 x y f = f (x - 1) y *> f x (y - 1) *> f x (y + 1) *> f (x + 1) y
{-# INLINE neighbor4 #-}

mkGrid :: C.ByteString -> U.Vector Char
mkGrid bs = U.unfoldrN (C.length bs) C.uncons bs
