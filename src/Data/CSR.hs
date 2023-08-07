{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.CSR where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Buffer

data CSR a = CSR
  { numRowsCSR :: !Int
  , offsetCSR :: !(U.Vector Int)
  , bufferCSR :: !(U.Vector a)
  }

rowAt :: (U.Unbox a) => CSR a -> Int -> U.Vector a
rowAt CSR{..} i = U.unsafeSlice o (o' - o) bufferCSR
  where
    o = U.unsafeIndex offsetCSR i
    o' = U.unsafeIndex offsetCSR (i + 1)
{-# INLINE rowAt #-}

accumulateToCSR ::
  (U.Unbox a) =>
  -- | num rows
  Int ->
  -- | buffer size
  Int ->
  U.Vector (Int, a) ->
  CSR a
accumulateToCSR n m v = createCSR n m $ \builder -> do
  U.forM_ v $ \(i, x) -> do
    pushCSRB (i, x) builder

createCSR ::
  (U.Unbox a) =>
  -- | num rows
  Int ->
  -- | buffer size
  Int ->
  (forall s. CSRBuilder s a -> ST s ()) ->
  CSR a
createCSR n m run = runST $ do
  builder <- newCSRBuilder n m
  run builder
  buildCSR builder

data CSRBuilder s a = CSRBuilder
  { numRowsCSRB :: !Int
  , queueCSRB :: !(Buffer s (Int, a))
  , outDegCSRB :: !(UM.MVector s Int)
  }

newCSRBuilder ::
  (U.Unbox a, PrimMonad m) =>
  -- | num rows
  Int ->
  -- | buffer size
  Int ->
  m (CSRBuilder (PrimState m) a)
newCSRBuilder numRows bufferSize =
  CSRBuilder numRows
    <$> newBuffer bufferSize
    <*> UM.replicate numRows 0
{-# INLINE newCSRBuilder #-}

buildCSR ::
  (U.Unbox a, PrimMonad m) =>
  CSRBuilder (PrimState m) a ->
  m (CSR a)
buildCSR CSRBuilder{..} = do
  m <- lengthBuffer queueCSRB
  offsetCSR <- U.scanl' (+) 0 <$> U.freeze outDegCSRB
  moffset <- U.thaw offsetCSR
  mbuffer <- UM.unsafeNew m
  fix $ \loop -> do
    popFront queueCSRB >>= \case
      Just (i, x) -> do
        pos <- UM.unsafeRead moffset i
        UM.unsafeWrite moffset i (pos + 1)
        UM.unsafeWrite mbuffer pos x
        loop
      Nothing -> return ()
  bufferCSR <- U.unsafeFreeze mbuffer
  return $ CSR{numRowsCSR = numRowsCSRB, ..}
{-# INLINE buildCSR #-}

pushCSRB ::
  (U.Unbox a, PrimMonad m) =>
  (Int, a) ->
  CSRBuilder (PrimState m) a ->
  m ()
pushCSRB (i, x) CSRBuilder{..} = do
  pushBack (i, x) queueCSRB
  UM.unsafeModify outDegCSRB (+ 1) i
{-# INLINE pushCSRB #-}
