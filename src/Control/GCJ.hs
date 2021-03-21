{-# LANGUAGE LambdaCase #-}

module Control.GCJ where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.IO
import System.Process

import Control.Monad.Interactive

formatGCJ :: Int -> String
formatGCJ i = "Case #" <> shows i ": "

withGCJ :: IO () -> IO ()
withGCJ f =
  getArgs >>= \case
    ["--debug"] -> f
    [] -> do
      t <- readLn
      mapM_ ((*> f) . putStr . formatGCJ) [1 .. t]
    args -> error $ show args

withGCJInteractive :: ([Int] -> Interactive a) -> IO ()
withGCJInteractive f = withInteractive $ do
  (t : params) <- map read . words <$> lift recvLine
  foldr ((<|>) . const (f params)) empty [1 .. t]
