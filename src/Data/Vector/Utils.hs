{-# LANGUAGE FlexibleContexts #-}

module Data.Vector.Utils where

import qualified Data.Vector.Generic as G

chunks :: (G.Vector u a, G.Vector v (u a)) => Int -> u a -> v (u a)
chunks n = G.unfoldr $ \u ->
  case G.null u of
    True -> Nothing
    False -> Just $ G.splitAt n u

group :: (Eq a, G.Vector u a, G.Vector v (u a)) => u a -> v (u a)
group = groupBy (==)

groupBy :: (G.Vector u a, G.Vector v (u a)) => (a -> a -> Bool) -> u a -> v (u a)
groupBy eq = G.unfoldr $ \u ->
  case G.null u of
    True -> Nothing
    False -> case G.unsafeHead u of
      u0 -> Just $ G.span (eq u0) u

pairs :: (G.Vector v a, G.Vector v (a, a)) => v a -> v (a, a)
pairs = G.unfoldr $ \v ->
  case G.length v >= 2 of
    True -> Just ((G.unsafeIndex v 0, G.unsafeIndex v 1), G.drop 2 v)
    False -> Nothing

pairsN :: (G.Vector v a, G.Vector v (a, a)) => Int -> v a -> v (a, a)
pairsN n = G.unfoldrN n $ \v ->
  case G.length v >= 2 of
    True -> Just ((G.unsafeIndex v 0, G.unsafeIndex v 1), G.drop 2 v)
    False -> Nothing

triples :: (G.Vector v a, G.Vector v (a, a, a)) => v a -> v (a, a, a)
triples = G.unfoldr $ \v ->
  case G.length v >= 3 of
    True -> Just ((G.unsafeIndex v 0, G.unsafeIndex v 1, G.unsafeIndex v 2), G.drop 3 v)
    False -> Nothing

triplesN :: (G.Vector v a, G.Vector v (a, a, a)) => Int -> v a -> v (a, a, a)
triplesN n = G.unfoldrN n $ \v ->
  case G.length v >= 3 of
    True -> Just ((G.unsafeIndex v 0, G.unsafeIndex v 1, G.unsafeIndex v 2), G.drop 3 v)
    False -> Nothing
