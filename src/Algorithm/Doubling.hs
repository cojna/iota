{-# LANGUAGE BangPatterns #-}

module Algorithm.Doubling where

import Data.Bits
import qualified Data.Foldable as F
import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

newtype Doubling a = Doubling {getDoubling :: U.Vector (Int, a)}

instance (Semigroup a, U.Unbox a) => Semigroup (Doubling a) where
  (Doubling next0) <> (Doubling next1) =
    Doubling $
      U.map
        ( \(nv, x) ->
            let (nnv, y) = U.unsafeIndex next1 nv
                !z = x <> y
             in (nnv, z)
        )
        next0
  {-# INLINE (<>) #-}

buildDoublingTable ::
  (Semigroup a, U.Unbox a) =>
  Doubling a ->
  V.Vector (Doubling a)
buildDoublingTable = V.iterateN 63 (\next -> next <> next)
{-# INLINE buildDoublingTable #-}

-- | /O(Mlog N)
doublingStepN ::
  (Semigroup a, U.Unbox a) =>
  Int ->
  Int ->
  a ->
  Doubling a ->
  (Int, a)
doublingStepN n x0 v0 next
  | n > 0 = let (xn, vn) = getDoubling (stimes n next) U.! x0 in (xn, v0 <> vn)
  | n == 0 = (x0, v0)
  | otherwise = error "doublingStepN: negative step"
{-# INLINE doublingStepN #-}

-- | /O(log N)
doublingStepQuery ::
  (Semigroup a, U.Unbox a) =>
  Int ->
  Int ->
  a ->
  V.Vector (Doubling a) ->
  (Int, a)
doublingStepQuery n x0 v0 table
  | n >= 0 = F.foldl' step (x0, v0) [0 .. 62]
  | otherwise = error "doublingStepQuery: negative step"
  where
    step (x, v) i
      | unsafeShiftR n i .&. 1 == 1 =
        let (xi, vi) = getDoubling (V.unsafeIndex table i) `U.unsafeIndex` x
            !v' = v <> vi
         in (xi, v')
      | otherwise = (x, v)
{-# INLINE doublingStepQuery #-}
