{-# LANGUAGE ConstraintKinds, ImplicitParams, LambdaCase, RankNTypes #-}

module Control.GCJ where

import           Control.Monad
import           System.Environment
import           System.IO
import           System.Process

formatGCJ :: Int -> String
formatGCJ i = "Case #" <> shows i ": "

withGCJ :: IO () -> IO ()
withGCJ f = getArgs >>= \case
    ["--debug"] -> f
    [] -> do
        t <- readLn
        mapM_ ((*> f) . putStrLn . formatGCJ) [1..t]
    args -> error $ show args

data InteractiveJudge = InteractiveJudge
    { hin :: Handle
    , hout :: Handle
    }

type HasInteractiveJudge = (?interactiveJudge :: InteractiveJudge)

withInteractiveJudge :: (HasInteractiveJudge => IO ()) -> IO ()
withInteractiveJudge f = do
    judge <- newInteractiveJudge
    let ?interactiveJudge = judge
    (t:_) <- map read.words <$> recvLine
    mapM_ ((*> f) . hPutStrLn stderr . formatGCJ) [1..t]

newInteractiveJudge :: IO InteractiveJudge
newInteractiveJudge = getArgs >>= \case
    [] -> return $ InteractiveJudge stdin stdout
    args -> do
        (Just i, Just o, _, _) <- createProcess
            (shell . unwords $ "python3 local_testing_tool.py": args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                }
        return $ InteractiveJudge o i

send :: (Show a, HasInteractiveJudge) => a -> IO ()
send = sendStrLn . show

sendStr :: HasInteractiveJudge => String -> IO ()
sendStr cs = do
    hPutStr (hout ?interactiveJudge) cs
    hFlush (hout ?interactiveJudge)

sendStrLn :: HasInteractiveJudge => String -> IO ()
sendStrLn cs = do
    hPutStrLn (hout ?interactiveJudge) cs
    hFlush (hout ?interactiveJudge)

recvLine :: HasInteractiveJudge => IO String
recvLine = hGetLine (hin ?interactiveJudge)

recvLn :: (Read a, HasInteractiveJudge) => IO a
recvLn = read <$> recvLine
