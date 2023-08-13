module Data.Vector.Utils where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

import My.Prelude (rep)

chunks :: (G.Vector u a, G.Vector v (u a)) => Int -> u a -> v (u a)
chunks n = G.unfoldr $ \u ->
  if G.null u then Nothing else Just $ G.splitAt n u

group :: (Eq a, G.Vector u a, G.Vector v (u a)) => u a -> v (u a)
group = groupBy (==)

groupBy :: (G.Vector u a, G.Vector v (u a)) => (a -> a -> Bool) -> u a -> v (u a)
groupBy eq = G.unfoldr $ \u ->
  if G.null u
    then Nothing
    else case G.unsafeHead u of
      u0 -> Just $ G.span (eq u0) u

tuples2 :: (G.Vector v a, G.Vector v (a, a)) => v a -> v (a, a)
tuples2 = G.unfoldr $ \v ->
  if G.length v >= 2
    then
      ( let !x = G.unsafeIndex v 0
            !y = G.unsafeIndex v 1
         in Just ((x, y), G.drop 2 v)
      )
    else Nothing

tuples2N :: (G.Vector v a, G.Vector v (a, a)) => Int -> v a -> v (a, a)
tuples2N n = G.unfoldrExactN n $ \v ->
  let !x = G.unsafeIndex v 0
      !y = G.unsafeIndex v 1
   in ((x, y), G.drop 2 v)

tuples3 :: (G.Vector v a, G.Vector v (a, a, a)) => v a -> v (a, a, a)
tuples3 = G.unfoldr $ \v ->
  if G.length v >= 3
    then
      ( let !x = G.unsafeIndex v 0
            !y = G.unsafeIndex v 1
            !z = G.unsafeIndex v 2
         in Just ((x, y, z), G.drop 3 v)
      )
    else Nothing

tuples3N :: (G.Vector v a, G.Vector v (a, a, a)) => Int -> v a -> v (a, a, a)
tuples3N n = G.unfoldrExactN n $ \v ->
  let !x = G.unsafeIndex v 0
      !y = G.unsafeIndex v 1
      !z = G.unsafeIndex v 2
   in ((x, y, z), G.drop 3 v)

tuples4 :: (G.Vector v a, G.Vector v (a, a, a, a)) => v a -> v (a, a, a, a)
tuples4 = G.unfoldr $ \v ->
  if G.length v >= 3
    then
      ( let !x = G.unsafeIndex v 0
            !y = G.unsafeIndex v 1
            !z = G.unsafeIndex v 2
            !w = G.unsafeIndex v 3
         in Just ((x, y, z, w), G.drop 4 v)
      )
    else Nothing

tuples4N ::
  (G.Vector v a, G.Vector v (a, a, a, a)) =>
  Int ->
  v a ->
  v (a, a, a, a)
tuples4N n = G.unfoldrExactN n $ \v ->
  let !x = G.unsafeIndex v 0
      !y = G.unsafeIndex v 1
      !z = G.unsafeIndex v 2
      !w = G.unsafeIndex v 3
   in ((x, y, z, w), G.drop 4 v)

{- |
>>> import qualified Data.Vector.Unboxed as U
>>> transpose 2 3 (U.fromList "abcdef")
"adbecf"
-}
transpose :: (G.Vector v a) => Int -> Int -> v a -> v a
transpose h w vec = G.create $ do
  buf <- GM.unsafeNew (w * h)
  rep h $ \x -> do
    G.foldM'_
      ( \pos mxy -> do
          GM.write buf pos mxy
          pure $! pos + h
      )
      x
      (G.slice (x * w) w vec)
  return buf
