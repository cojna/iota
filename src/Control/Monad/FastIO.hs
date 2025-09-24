{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Monad.FastIO where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.Trans
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.ByteString.Internal as B
import Data.Function
import Foreign
import GHC.Exts
import GHC.IO
import GHC.Word
import System.IO

newtype Solver a = Solver
  { runSolver ::
      Addr# ->
      Addr# ->
      State# RealWorld ->
      (# Addr#, Addr#, State# RealWorld, a #)
  }

withSolver :: Int -> Solver () -> IO ()
withSolver bufSize f = do
  buf <- mallocForeignPtrBytes bufSize
  withForeignPtr buf $ \ibuf@(Ptr ibuf#) -> do
    isize <- hGetBufNonBlocking stdin ibuf bufSize
    pokeElemOff (plusPtr ibuf isize) 0 (10 :: Word8) -- sentinel ('\n')
    let !obuf@(Ptr obuf#) = plusPtr ibuf (isize + 1)
    obuf' <-
      IO
        ( \s -> case runSolver f ibuf# obuf# s of
            (# _ibuf'#, obuf'#, s'#, _ #) -> (# s'#, Ptr obuf'# #)
        )
    _osize <- hPutBufNonBlocking stdout obuf (minusPtr obuf' obuf)
    hFlush stdout
{-# INLINE withSolver #-}

defaultBufferSize :: Int
defaultBufferSize = 32 * 1024 * 1024 - 2 * 8

get :: PrimReader a -> Solver a
get = liftPrimReader
{-# INLINE get #-}

put :: (PrimWrite a) => a -> Solver ()
put = liftPrimWriter . primWrite
{-# INLINE put #-}

putLn :: (PrimWrite a) => a -> Solver ()
putLn x = put x *> put '\n'
{-# INLINE putLn #-}

putSp :: (PrimWrite a) => a -> Solver ()
putSp x = put x *> put ' '
{-# INLINE putSp #-}

instance Functor Solver where
  fmap f mx = Solver $ \ibuf obuf s ->
    case runSolver mx ibuf obuf s of
      (# ibuf', obuf', s', x #) -> (# ibuf', obuf', s', f x #)
  {-# INLINE fmap #-}

instance Applicative Solver where
  pure x = Solver $ \ibuf obuf s -> (# ibuf, obuf, s, x #)
  {-# INLINE pure #-}
  mf <*> mx = Solver $ \ibuf obuf s ->
    case runSolver mf ibuf obuf s of
      (# ibuf', obuf', s', f #) -> case runSolver mx ibuf' obuf' s' of
        (# ibuf'', obuf'', s'', x #) -> (# ibuf'', obuf'', s'', f x #)
  {-# INLINE (<*>) #-}
  liftA2 f mx my = Solver $ \ibuf obuf s ->
    case runSolver mx ibuf obuf s of
      (# ibuf', obuf', s', x #) -> case runSolver my ibuf' obuf' s' of
        (# ibuf'', obuf'', s'', y #) -> (# ibuf'', obuf'', s'', f x y #)
  {-# INLINE liftA2 #-}

instance Monad Solver where
  mx >>= f = Solver $ \ibuf obuf s ->
    case runSolver mx ibuf obuf s of
      (# ibuf', obuf', s', x #) -> runSolver (f x) ibuf' obuf' s'
  {-# INLINE (>>=) #-}

instance MonadIO Solver where
  liftIO f = Solver $ \ibuf obuf s ->
    case unIO f s of
      (# s', x #) -> (# ibuf, obuf, s', x #)
  {-# INLINE liftIO #-}

instance PrimMonad Solver where
  type PrimState Solver = RealWorld
  primitive f = Solver $ \ibuf obuf s ->
    case f s of
      (# s', x #) -> (# ibuf, obuf, s', x #)
  {-# INLINE primitive #-}

newtype PrimReader a = PrimReader {runPrimReader :: Addr# -> (# Addr#, a #)}

instance Functor PrimReader where
  fmap f mx = PrimReader $ \ibuf -> case runPrimReader mx ibuf of
    (# ibuf', x #) -> (# ibuf', f x #)
  {-# INLINE fmap #-}

instance Applicative PrimReader where
  pure x = PrimReader $ \ibuf -> (# ibuf, x #)
  {-# INLINE pure #-}
  mf <*> mx = PrimReader $ \ibuf -> case runPrimReader mf ibuf of
    (# ibuf', f #) -> case runPrimReader mx ibuf' of
      (# ibuf'', x #) -> (# ibuf'', f x #)
  {-# INLINE (<*>) #-}
  liftA2 f mx my = PrimReader $ \ibuf -> case runPrimReader mx ibuf of
    (# ibuf', x #) -> case runPrimReader my ibuf' of
      (# ibuf'', y #) -> (# ibuf'', f x y #)
  {-# INLINE liftA2 #-}

instance Monad PrimReader where
  mx >>= f = PrimReader $ \ibuf -> case runPrimReader mx ibuf of
    (# ibuf', x #) -> runPrimReader (f x) ibuf'
  {-# INLINE (>>=) #-}

liftPrimReader :: PrimReader a -> Solver a
liftPrimReader r = Solver $ \ibuf obuf s ->
  case runPrimReader r ibuf of
    (# ibuf', x #) -> (# ibuf', obuf, s, x #)
{-# INLINE liftPrimReader #-}

word8 :: PrimReader Word8
word8 = PrimReader $ \ptr ->
  (# plusAddr# ptr 2#, W8# (indexWord8OffAddr# ptr 0#) #)
{-# INLINE word8 #-}

char :: PrimReader Char
char = PrimReader $ \ptr ->
  (# plusAddr# ptr 2#, B.w2c (W8# (indexWord8OffAddr# ptr 0#)) #)
{-# INLINE char #-}

int :: PrimReader Int
int = PrimReader $ \o ->
  let sgn = eqWord# (indexWord8OffAddr# o 0#) 0x2d##
   in fix
        ( \loop !acc !ptr ->
            case indexWord8OffAddr# ptr 0# of
              c
                | isTrue# (geWord# c 0x30##) ->
                  loop (andI# (word2Int# c) 15# +# 10# *# acc) (plusAddr# ptr 1#)
                | isTrue# sgn -> (# plusAddr# ptr 1#, I# (negateInt# acc) #)
                | otherwise -> (# plusAddr# ptr 1#, I# acc #)
        )
        0#
        (plusAddr# o sgn)
{-# INLINE int #-}

newtype PrimWriter a = PrimWriter
  { runPrimWriter ::
      Addr# ->
      State# RealWorld ->
      (# Addr#, State# RealWorld, a #)
  }

instance Functor PrimWriter where
  fmap f mx = PrimWriter $ \obuf s ->
    case runPrimWriter mx obuf s of
      (# obuf', s', x #) -> (# obuf', s', f x #)
  {-# INLINE fmap #-}

instance Applicative PrimWriter where
  pure x = PrimWriter $ \obuf s -> (# obuf, s, x #)
  {-# INLINE pure #-}
  mf <*> mx = PrimWriter $ \obuf s ->
    case runPrimWriter mf obuf s of
      (# obuf', s', f #) -> case runPrimWriter mx obuf' s' of
        (# obuf'', s'', x #) -> (# obuf'', s'', f x #)
  {-# INLINE (<*>) #-}
  liftA2 f mx my = PrimWriter $ \obuf s ->
    case runPrimWriter mx obuf s of
      (# obuf', s', x #) -> case runPrimWriter my obuf' s' of
        (# obuf'', s'', y #) -> (# obuf'', s'', f x y #)
  {-# INLINE liftA2 #-}

instance Monad PrimWriter where
  mx >>= f = PrimWriter $ \obuf s ->
    case runPrimWriter mx obuf s of
      (# obuf', s', x #) -> runPrimWriter (f x) obuf' s'
  {-# INLINE (>>=) #-}

instance MonadIO PrimWriter where
  liftIO f = PrimWriter $ \obuf s ->
    case unIO f s of
      (# s', x #) -> (# obuf, s', x #)
  {-# INLINE liftIO #-}

instance PrimMonad PrimWriter where
  type PrimState PrimWriter = RealWorld
  primitive f = PrimWriter $ \obuf s ->
    case f s of
      (# s', x #) -> (# obuf, s', x #)
  {-# INLINE primitive #-}

liftPrimWriter :: PrimWriter a -> Solver a
liftPrimWriter w = Solver $ \ibuf obuf s ->
  case runPrimWriter w obuf s of
    (# obuf', s', x #) -> (# ibuf, obuf', s', x #)
{-# INLINE liftPrimWriter #-}

class PrimWrite a where
  primWrite :: a -> PrimWriter ()

instance PrimWrite Int where
  primWrite x = PrimWriter $ \obuf s ->
    case unIO (BP.runB BP.intDec x (Ptr obuf)) s of
      (# s', Ptr obuf' #) -> (# obuf', s', () #)
  {-# INLINE primWrite #-}

instance PrimWrite Word8 where
  primWrite (W8# x) = PrimWriter $ \obuf s ->
    case writeWord8OffAddr# obuf 0# x s of
      s' -> (# plusAddr# obuf 1#, s', () #)
  {-# INLINE primWrite #-}

instance PrimWrite Char where
  primWrite = primWrite . B.c2w
  {-# INLINE primWrite #-}

instance (PrimWrite a) => PrimWrite [a] where
  primWrite = mapM_ primWrite
  {-# INLINE primWrite #-}
