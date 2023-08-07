module Data.Vector.Compress where

import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

--
import Data.Vector.Sort.Radix

compress :: U.Vector Int -> U.Vector Int
compress vec = U.create $ do
  mvec <- UM.unsafeNew (U.length vec)
  U.mapM_ (\(i, x) -> UM.unsafeWrite mvec (x .&. 0xffffffff) i)
    . U.postscanl'
      ( \(!i, !x) y ->
          if unsafeShiftR x 32 == unsafeShiftR y 32
            then (i, y)
            else (i + 1, y)
      )
      (-1, -1)
    . radixSortInt
    $ U.imap (\i x -> unsafeShiftL x 32 .|. i) vec
  return mvec
{-# INLINE compress #-}
