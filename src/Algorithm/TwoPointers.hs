{-# LANGUAGE LambdaCase #-}

module Algorithm.TwoPointers where

import Control.Monad.Primitive
import Data.Function
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import qualified Data.Vector.Fusion.Bundle.Size as Bundle
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- | @[l, r)@
data Window a = Window
  { leftW :: !Int
  , rightW :: !Int
  , contextW :: !a
  }

enumerateTwoPointers ::
  (Monad m) =>
  -- | size
  Int ->
  -- | shrinkL (@l < r@)
  (Window a -> m a) ->
  -- | tryExtendR (@l <= r@)
  (Window a -> m (Maybe a)) ->
  -- | context for the empty
  a ->
  m Int
enumerateTwoPointers n shrinkL tryExtendR x0 = do
  fix
    ( \loop !acc w@(Window l r _) -> do
        if r < n
          then do
            tryExtendR w >>= \case
              Nothing
                | l < r -> shrinkL w >>= loop (acc + r - l) . Window (l + 1) r
                | otherwise -> loop acc (Window (l + 1) (l + 1) x0)
              Just w' -> loop acc (Window l (r + 1) w')
          else pure $ acc + (r - l) * (r - l + 1) `quot` 2
    )
    0
    (Window 0 0 x0)
{-# INLINE enumerateTwoPointers #-}

maxLengthTwoPointers ::
  (Monad m) =>
  -- | size
  Int ->
  -- | shrinkL (@l < r@)
  (Window a -> m a) ->
  -- | tryExtendR (@l <= r@)
  (Window a -> m (Maybe a)) ->
  -- | context for the empty
  a ->
  m Int
maxLengthTwoPointers n shrinkL tryExtendR x0 = do
  fix
    ( \loop !acc w@(Window l r _) -> do
        if r < n
          then do
            tryExtendR w >>= \case
              Nothing
                | l < r -> shrinkL w >>= loop (max acc (r - l)) . Window (l + 1) r
                | otherwise -> loop acc (Window (l + 1) (l + 1) x0)
              Just w' -> loop acc (Window l (r + 1) w')
          else pure $ max acc (r - l)
    )
    0
    (Window 0 0 x0)
{-# INLINE maxLengthTwoPointers #-}

runTwoPointersStream ::
  (Monad m) =>
  -- | start
  Int ->
  -- | end
  Int ->
  -- | shrinkL (@l < r@)
  (Window a -> m a) ->
  -- | tryExtendR (@l <= r@)
  (Window a -> m (Maybe a)) ->
  a ->
  -- | context for the empty
  MS.Stream m (Window a)
runTwoPointersStream l0 r0 shrinkL tryExtendR x0 = MS.Stream step (Window l0 l0 x0)
  where
    step w@(Window l r _)
      | r < r0 = do
          tryExtendR w >>= \case
            Nothing
              | l < r -> do
                  MS.Yield w . Window (l + 1) r <$> shrinkL w
              | otherwise -> do
                  return $ MS.Yield w (Window (l + 1) (l + 1) x0)
            Just x' -> return $ MS.Skip (Window l (r + 1) x')
      | l < r0 = MS.Yield w . Window (l + 1) r <$> shrinkL w
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] runTwoPointersStream #-}

runTwoPointersM ::
  (PrimMonad m, G.Vector v (Window a)) =>
  -- | start
  Int ->
  -- | end
  Int ->
  -- | shrinkL (@l < r@)
  (Window a -> m a) ->
  -- | tryExtendR (@l <= r@)
  (Window a -> m (Maybe a)) ->
  a ->
  -- | context for the empty
  m (v (Window a))
runTwoPointersM l0 r0 shrinkL tryExtendR x0 =
  GM.munstream
    ( MBundle.fromStream
        (runTwoPointersStream l0 r0 shrinkL tryExtendR x0)
        (Bundle.Exact (r0 - l0))
    )
    >>= G.unsafeFreeze
{-# INLINE runTwoPointersM #-}

runTwoPointers ::
  (G.Vector v (Window a)) =>
  -- | start
  Int ->
  -- | end
  Int ->
  -- | shrinkL (@l < r@)
  (Window a -> a) ->
  -- | tryExtendR (@l <= r@)
  (Window a -> Maybe a) ->
  -- | context for the empty
  a ->
  v (Window a)
runTwoPointers l0 r0 shrinkL tryExtendR x0 =
  G.unstream
    ( MBundle.fromStream
        (runTwoPointersStream l0 r0 (pure . shrinkL) (pure . tryExtendR) x0)
        (Bundle.Exact (r0 - l0))
    )
{-# INLINE runTwoPointers #-}
