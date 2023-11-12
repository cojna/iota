{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module System.Random.Utils where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic.Mutable as GM
import System.Random.Stateful

import My.Prelude

-- | global StdGen
newStdGenPrim :: (PrimMonad m) => m StdGen
newStdGenPrim = unsafeIOToPrim newStdGen
{-# INLINE newStdGenPrim #-}

newtype PrimGen g = PrimGen {unPrimGen :: g}
  deriving newtype (RandomGen)

newtype PrimGenM g s = PrimGenM {unPrimGenM :: MutVar s g}

newPrimGenM :: (PrimMonad m) => g -> m (PrimGenM g (PrimState m))
newPrimGenM g = PrimGenM <$> newMutVar g

applyPrimGen ::
  (PrimMonad m) =>
  (g -> (a, g)) ->
  PrimGenM g (PrimState m) ->
  m a
applyPrimGen f (PrimGenM ref) = do
  (!a, !g) <- f <$> readMutVar ref
  writeMutVar ref g
  return a
{-# INLINE applyPrimGen #-}

instance
  (RandomGen g, s ~ PrimState m, PrimMonad m) =>
  StatefulGen (PrimGenM g s) m
  where
  uniformWord32R = applyPrimGen . genWord32R
  {-# INLINE uniformWord32R #-}
  uniformWord64R = applyPrimGen . genWord64R
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyPrimGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyPrimGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyPrimGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyPrimGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString = applyPrimGen . genShortByteString
  {-# INLINE uniformShortByteString #-}

instance
  (RandomGen g, s ~ PrimState m, PrimMonad m) =>
  RandomGenM (PrimGenM g s) g m
  where
  applyRandomGenM = applyPrimGen

instance
  (RandomGen g, PrimMonad m) =>
  FrozenGen (PrimGen g) m
  where
  type MutableGen (PrimGen g) m = PrimGenM g (PrimState m)
  freezeGen (PrimGenM ref) = PrimGen <$> readMutVar ref
  thawGen (PrimGen g) = newPrimGenM g

withGlobalStdGen ::
  (PrimMonad m) =>
  (PrimGenM StdGen (PrimState m) -> m a) ->
  m (a, StdGen)
withGlobalStdGen f = do
  rng <- newStdGenPrim
  fmap unPrimGen <$> withMutableGen (PrimGen rng) f

withGlobalStdGen_ ::
  (PrimMonad m) =>
  (PrimGenM StdGen (PrimState m) -> m a) ->
  m a
withGlobalStdGen_ f = do
  rng <- newStdGenPrim
  withMutableGen_ (PrimGen rng) f

{- |
>>> withFixedStdGen 123 (uniformRM @Int (1, 6))
(1,StdGen {unStdGen = SMGen 3794253433779795923 13032462758197477675})
>>> withFixedStdGen 1 (uniformRM @Int (1, 6))
(6,StdGen {unStdGen = SMGen 4999253871718377453 10451216379200822465})
-}
withFixedStdGen ::
  (PrimMonad m) =>
  Int ->
  (PrimGenM StdGen (PrimState m) -> m a) ->
  m (a, StdGen)
withFixedStdGen seed f = do
  fmap unPrimGen <$> withMutableGen (PrimGen (mkStdGen seed)) f

{- |
>>> withFixedStdGen_ 123 (uniformRM @Int (1, 6))
1
>>> withFixedStdGen_ 1 (uniformRM @Int (1, 6))
6
-}
withFixedStdGen_ ::
  (PrimMonad m) =>
  Int ->
  (PrimGenM StdGen (PrimState m) -> m a) ->
  m a
withFixedStdGen_ seed f = do
  withMutableGen_ (PrimGen (mkStdGen seed)) f

{- |
>>> import qualified Data.Vector.Unboxed as U
>>> U.modify (shuffle (mkStdGen 123)) $ U.fromList "abcdef"
"fcdbea"
-}
shuffle ::
  (PrimMonad m, GM.MVector mv a, RandomGen g) =>
  g ->
  mv (PrimState m) a ->
  m ()
shuffle rng0 mv = do
  void
    $ MS.foldM'
      ( \rng i -> do
          case genWord64R (fromIntegral i) rng of
            (j, rng') -> do
              GM.unsafeSwap mv i (fromIntegral j)
              pure rng'
      )
      rng0
      (GM.length mv >.. 0)
{-# INLINE shuffle #-}
