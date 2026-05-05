{-# LANGUAGE LambdaCase #-}

module Algorithm.TwoPointers where

import Data.Function

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
