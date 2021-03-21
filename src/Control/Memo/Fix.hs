{-# LANGUAGE LambdaCase #-}

module Control.Memo.Fix where

import Control.Monad.State
import Data.Function
import Data.Functor (($>))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

{- |
 >>> :{
 let fib memo 0 = 0
     fib memo 1 = 1
     fib memo i = memo (i - 1) + memo (i - 2)
 in memoFix 100 fib 50
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
 >>>  :{
 let fibM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
     fibM _ 0 = pure 0
     fibM _ 1 = pure 1
     fibM memo i = (+) <$> memo (i - 1) <*> memo (i - 2)
 in memoFixMap fibM 50
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
 >>>  :{
 let fibM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
     fibM _ 0 = pure 0
     fibM _ 1 = pure 1
     fibM memo i = (+) <$> memo (i - 1) <*> memo (i - 2)
 in memoFixIntMap fibM 50
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
