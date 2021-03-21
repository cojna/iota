{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}

module My.Prelude.Safe where

import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Word

rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep !n f = foldr ((>>) . f) (return ()) [0 .. n -1]
{-# INLINE rep #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev !n f = foldr ((>>) . f . negate) (return ()) [1 - n .. 0]
{-# INLINE rev #-}

type Parser a = StateT C.ByteString Maybe a

runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}

int :: Parser Int
int = StateT $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}

int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}

char :: Parser Char
char = StateT C.uncons
{-# INLINE char #-}

byte :: Parser Word8
byte = StateT B.uncons
{-# INLINE byte #-}
