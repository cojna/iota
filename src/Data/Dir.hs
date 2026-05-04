{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Dir where

import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word

data Dir = L | R | U | D
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance U.IsoUnbox Dir Word8 where
  toURepr = fromIntegral . fromEnum
  {-# INLINE toURepr #-}
  fromURepr = toEnum . fromIntegral
  {-# INLINE fromURepr #-}

newtype instance UM.MVector s Dir = MV_Dir (UM.MVector s Word8)
newtype instance U.Vector Dir = V_Dir (U.Vector Word8)
deriving via (Dir `U.As` Word8) instance GM.MVector U.MVector Dir
deriving via (Dir `U.As` Word8) instance G.Vector U.Vector Dir
instance U.Unbox Dir

{- |
>>> enumerateDir
[L,R,U,D]
-}
enumerateDir :: [Dir]
enumerateDir = [L, R, U, D]

{- |
>>> opposite L
R
>>> opposite R
L
>>> opposite U
D
>>> opposite D
U
-}
opposite :: Dir -> Dir
opposite = toEnum . xor 1 . fromEnum
{-# INLINE opposite #-}

dirToChar :: Dir -> Char
dirToChar L = 'L'
dirToChar R = 'R'
dirToChar U = 'U'
dirToChar D = 'D'

charToDir :: Char -> Dir
charToDir 'L' = L
charToDir 'R' = R
charToDir 'U' = U
charToDir 'D' = D
charToDir c = error $ "invalid dir: " <> [c]

{- |
>>> dirMove (2, 3) U
(1,3)
>>> dirMove (0, 0) U
(-1,0)
>>> dirMove (0, 0) R
(0,1)
>>> map (dirMove (2, 3)) enumerateDir
[(2,2),(2,4),(1,3),(3,3)]
-}
dirMove :: (Int, Int) -> Dir -> (Int, Int)
dirMove (!x, !y) L = let !y' = y - 1 in (x, y')
dirMove (!x, !y) R = let !y' = y + 1 in (x, y')
dirMove (!x, !y) U = let !x' = x - 1 in (x', y)
dirMove (!x, !y) D = let !x' = x + 1 in (x', y)

{- |
>>> dir4Moves (2, 3)
[(2,2),(2,4),(1,3),(3,3)]
>>> dir4Moves (0, 1)
[(0,0),(0,2),(-1,1),(1,1)]
-}
dir4Moves :: (Int, Int) -> [(Int, Int)]
dir4Moves (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
{-# INLINE dir4Moves #-}

{- |
>>> dirDiff U
(-1,0)
>>> dirDiff R
(0,1)
-}
dirDiff :: Dir -> (Int, Int)
dirDiff L = (0, -1)
dirDiff R = (0, 1)
dirDiff U = (-1, 0)
dirDiff D = (1, 0)

{- |
>>> moveToDir (0, 1) (0, 2)
R
>>> moveToDir (1, 2) (0, 2)
U
>>> moveToDir (0, 0) (1, 1)
*** Exception: invalid move: (1,1)
-}
moveToDir :: (Int, Int) -> (Int, Int) -> Dir
moveToDir (x, y) (x', y') = case (x' - x, y' - y) of
  (0, dy)
    | dy < 0 -> L
    | dy > 0 -> R
  (dx, 0)
    | dx < 0 -> U
    | dx > 0 -> D
  (dx, dy) -> error $ "invalid move: " <> show (dx, dy)
