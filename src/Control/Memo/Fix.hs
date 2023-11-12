{-# LANGUAGE LambdaCase #-}

module Control.Memo.Fix where

import Control.Monad.State
import Data.Functor (($>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

{- |
>>> :set -XLambdaCase
>>> :{
let memoFib = \fib -> \case
      0 -> 0
      1 -> 1
      i -> fib (i - 1) + fib (i - 2)
 in memoFix 100 memoFib 50
:}
12586269025
-}
memoFix ::
  -- | memo size
  Int ->
  ((Int -> a) -> Int -> a) ->
  Int ->
  a
memoFix n f = fix $ \memo -> (V.generate n (f memo) V.!)

{- |
>>> :set -XLambdaCase
>>> :{
let memoFibM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
    memoFibM = \fib -> \case
      0 -> pure 0
      1 -> pure 1
      i -> (+) <$> fib (i - 1) <*> fib (i - 2)
 in memoFixMap memoFibM 50
:}
12586269025
-}
memoFixMap ::
  (Ord k) =>
  ((k -> State (M.Map k a) a) -> k -> State (M.Map k a) a) ->
  k ->
  a
memoFixMap f k = flip evalState M.empty $ do
  flip fix k $ \memo x -> do
    gets (M.lookup x) >>= \case
      Just fx -> pure fx
      Nothing ->
        f memo x >>= \fx ->
          modify' (M.insert x fx) $> fx

{- |
>>> :set -XLambdaCase
>>> :{
let memoFibM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
    memoFibM = \fib -> \case
      0 -> pure 0
      1 -> pure 1
      i -> (+) <$> fib (i - 1) <*> fib (i - 2)
 in memoFixIntMap memoFibM 50
:}
12586269025
-}
memoFixIntMap ::
  ((Int -> State (IM.IntMap a) a) -> Int -> State (IM.IntMap a) a) ->
  Int ->
  a
memoFixIntMap f n = flip evalState IM.empty $ do
  flip fix n $ \memo x -> do
    gets (IM.lookup x) >>= \case
      Just fx -> pure fx
      Nothing ->
        f memo x >>= \fx ->
          modify' (IM.insert x fx) $> fx
