{-# LANGUAGE BangPatterns, MagicHash #-}

module Utils where

import           Control.Monad.State.Strict
import           Data.Bool
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as C
import           Data.Char
import           Data.Coerce
import           Data.Functor.Identity
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import           Data.Word
import           GHC.Exts

rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}

rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev !n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}

stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l
  where
    step x
        | x < r = return $ MS.Yield x (x + 1)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1)
  where
    step x
        | x >= l = return $ MS.Yield x (x - 1)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l
  where
    step x
        | x < r = return $ MS.Yield x (x + d)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream' #-}

infixl 8 `shiftRL`, `unsafeShiftRL`
shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}

unsafeShiftRL :: Int -> Int -> Int
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}

type Parser a = StateT C.ByteString Maybe a

runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}

int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}

int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}

char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}

byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}

skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}

-- | assert (p high)
lowerBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
lowerBoundM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid)
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE lowerBoundM #-}

-- | assert (p low)
upperBoundM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
upperBoundM low high p = do
    flg <- p high
    if flg
    then return high
    else subtract 1 <$!> lowerBoundM low high (fmap not.p)
{-# INLINE upperBoundM #-}

-- | assert (p high)
lowerBound :: Int -> Int -> (Int -> Bool) -> Int
lowerBound low high p = runIdentity (lowerBoundM low high (return . p))
{-# INLINE lowerBound #-}

-- | assert (p low)
upperBound :: Int -> Int -> (Int -> Bool) -> Int
upperBound low high p = runIdentity (upperBoundM low high (return . p))
{-# INLINE upperBound #-}
