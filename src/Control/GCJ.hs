{-# LANGUAGE CPP #-}

module Control.GCJ where

import           Control.Monad
import qualified System.IO      as IO
import           System.Process

runGCJ :: IO () -> IO ()
runGCJ main_ = do
    t <- readLn
    forM_ [1..t] $ \i -> do
        putStr $ "Case #" ++ shows i ": "
        main_

runInteractive :: (IO.Handle -> IO.Handle -> IO ()) -> IO ()
runInteractive action = do
#ifdef DEBUG
    (Just hout, Just hin, _, _) <- createProcess
        (shell "python3 testing_tool.py 0")
            { std_in = CreatePipe, std_out = CreatePipe}
#else
    let hin = IO.stdin
    let hout = IO.stdout
#endif
    t:_ <- map read.words <$> IO.hGetLine hin :: IO [Int]
    forM_ [1..t] $ \i -> do
        IO.hPutStrLn IO.stderr $ "Case #" ++ shows i ":"
        action hin hout
