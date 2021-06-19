{-# LANGUAGE BangPatterns #-}

module Data.Doubling where

import Data.Bits
import qualified Data.Foldable as F
import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

newtype Doubling a = Doubling {getDoubling :: U.Vector (Int, a)}

generateDoubling :: (U.Unbox a) => Int -> (Int -> (Int, a)) -> Doubling a
generateDoubling n f = Doubling $ U.generate n f

generateDoubling_ :: Int -> (Int -> Int) -> Doubling ()
generateDoubling_ n f = Doubling $ U.generate n (flip (,) () . f)

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

-- | /O(Mlog N)/
doublingStepN ::
  (Semigroup a, U.Unbox a) =>
  -- | n
  Int ->
  -- | initial state
  Int ->
  -- | initial value
  a ->
  Doubling a ->
  (Int, a)
doublingStepN n x0 v0 next
  | n > 0 = let (xn, vn) = getDoubling (stimes n next) U.! x0 in (xn, v0 <> vn)
  | n == 0 = (x0, v0)
  | otherwise = error "doublingStepN: negative step"
{-# INLINE doublingStepN #-}

doublingStepN_ ::
  -- | n
  Int ->
  -- | initial state
  Int ->
  Doubling () ->
  Int
doublingStepN_ n x0 next
  | n > 0 = fst $ getDoubling (stimes n next) U.! x0
  | n == 0 = x0
  | otherwise = error "doublingStepN_: negative step"
{-# INLINE doublingStepN_ #-}

newtype DoublingTable a = DoublingTable (V.Vector (Doubling a))

buildDoublingTable ::
  (Semigroup a, U.Unbox a) =>
  Doubling a ->
  DoublingTable a
buildDoublingTable = DoublingTable . V.iterateN 63 (\next -> next <> next)
{-# INLINE buildDoublingTable #-}

-- | /O(log N)/
doublingStepNQuery ::
  (Semigroup a, U.Unbox a) =>
  -- | n
  Int ->
  -- | initial state
  Int ->
  -- | initial value
  a ->
  DoublingTable a ->
  (Int, a)
doublingStepNQuery n x0 v0 (DoublingTable table)
  | n >= 0 = F.foldl' step (x0, v0) [0 .. 62]
  | otherwise = error "doublingStepQuery: negative step"
  where
    step (x, v) i
      | unsafeShiftR n i .&. 1 == 1 =
        let (xi, vi) = getDoubling (V.unsafeIndex table i) `U.unsafeIndex` x
            !v' = v <> vi
         in (xi, v')
      | otherwise = (x, v)
{-# INLINE doublingStepNQuery #-}
