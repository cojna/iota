{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Interactive where

import Control.Applicative
import qualified Control.Monad.Fail as Fail
import Control.Monad.Primitive
import Control.Monad.Reader
import qualified Data.List as L
import System.Environment
import System.IO
import System.Process

class (Monad m) => MonadInteractive m where
  {-# MINIMAL sendStr, recvLine #-}
  sendStr :: String -> m ()
  recvLine :: m String

  send :: (Show a) => a -> m ()
  send = sendStrLn . show

  sendStrLn :: String -> m ()
  sendStrLn cs = sendStr cs >> sendStr "\n"

  recvLn :: (Read a) => m a
  recvLn = read <$> recvLine

data InteractiveHandle = InteractiveHandle
  { hin :: Handle
  , hout :: Handle
  , mherr :: Maybe Handle
  }

createInteractiveHandle :: IO InteractiveHandle
createInteractiveHandle =
  getArgs >>= \case
    [] -> return $ InteractiveHandle stdin stdout Nothing
    args -> do
      let cmds = filter (not . L.isPrefixOf "-") args
      (Just hout, Just hin, _, _) <-
        createProcess
          (shell . unwords $ "python3 local_testing_tool.py" : cmds)
            { std_in = CreatePipe
            , std_out = CreatePipe
            }
      let mherr
            | "--verbose" `elem` args = Just stderr
            | otherwise = Nothing
      return $ InteractiveHandle{..}

withInteractiveHandle :: ReaderT InteractiveHandle IO a -> IO a
withInteractiveHandle f = createInteractiveHandle >>= runReaderT f

sendDebugFormat :: String -> String
sendDebugFormat = ("> " ++)

recvDebugFormat :: String -> String
recvDebugFormat = ("< " ++)

instance (MonadIO m) => MonadInteractive (ReaderT InteractiveHandle m) where
  sendStr cs = ReaderT $ \InteractiveHandle{..} ->
    liftIO $ do
      mapM_ (\herr -> hPutStr herr $ sendDebugFormat cs) mherr
      hPutStr hout cs
      hFlush hout

  sendStrLn cs = ReaderT $ \InteractiveHandle{..} ->
    liftIO $ do
      mapM_ (\herr -> hPutStrLn herr $ sendDebugFormat cs) mherr
      hPutStrLn hout cs
      hFlush hout
  recvLine = ReaderT $ \InteractiveHandle{..} ->
    liftIO $ do
      res <- hGetLine hin
      mapM_ (\herr -> hPutStrLn herr $ recvDebugFormat res) mherr
      return res

type Result m r = m r
type Accepted m r = Result m r
type Failed m r = Result m r
type Running a m r = a -> Result m r
type JudgeInternal m a r =
  Accepted m r -> Failed m r -> Running a m r -> Result m r

newtype Judge m a = Judge {unJudge :: forall r. JudgeInternal m a r}

instance Functor (Judge m) where
  fmap f m = Judge $ \ac wa k ->
    unJudge m ac wa (k . f)
  {-# INLINE fmap #-}

instance Applicative (Judge m) where
  pure x = Judge $ \_ _ k -> k x
  {-# INLINE pure #-}
  mf <*> mx = Judge $ \ac wa k ->
    unJudge mf ac wa $ \f ->
      unJudge mx ac wa (k . f)
  {-# INLINE (<*>) #-}

instance Alternative (Judge m) where
  empty = Judge $ \ac _ _ -> ac
  {-# INLINE empty #-}
  mx <|> my = Judge $ \ac wa k ->
    unJudge mx (unJudge my ac wa k) wa k
  {-# INLINE (<|>) #-}

instance Monad (Judge m) where
  mx >>= f = Judge $ \ac wa k ->
    unJudge mx ac wa $ \x ->
      unJudge (f x) ac wa k
  {-# INLINE (>>=) #-}

instance Fail.MonadFail (Judge m) where
  fail _ = Judge $ \_ wa _ -> wa
  {-# INLINE fail #-}

instance MonadTrans Judge where
  lift m = Judge $ \_ _ k -> m >>= k
  {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (Judge m) where
  type PrimState (Judge m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

type Interactive a = Judge (ReaderT InteractiveHandle IO) a

interactive ::
  (forall r. JudgeInternal (ReaderT InteractiveHandle IO) a r) ->
  Interactive a
interactive = Judge

runInteractive_ :: Interactive a -> ReaderT InteractiveHandle IO ()
runInteractive_ m = unJudge m (return ()) (return ()) (const $ return ())

withInteractive :: Interactive a -> IO ()
withInteractive = withInteractiveHandle . runInteractive_
