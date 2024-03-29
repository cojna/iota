module Data.Graph.Dense.TSP where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Coerce
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import Data.Vector.Fusion.Util
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.BitSet
import Data.Graph.Dense
import My.Prelude (rep, (..<), (>..))

data TSPResult a = TSPResult
  { resultTSP :: !a
  , freezedTSP :: U.Vector a
  }

{- | Traveling Salesman Problem

/O(n^2 2^n)/

>>> resultTSP . runTSP 0x3f3f3f3f $ fromListDG @Int [[0,1,999],[999,0,2],[4,999,0]]
7
>>> resultTSP . runTSP 0x3f3f3f3f $ fromListDG @Int [[0,1,1],[1,0,8],[1,999,0]]
10
>>> resultTSP . runTSP 0x3f3f3f3f $ fromListDG @Int [[0]]
0
>>> resultTSP . runTSP 999 $ fromListDG @Int [[0,-1,999],[999,0,1],[1,999,0]]
1
>>> resultTSP . runTSP 999 $ fromListDG @Int [[0,-1,999],[999,0,-1],[999,999,0]]
997
-}
runTSP ::
  (U.Unbox w, Num w, Eq w, Ord w) =>
  -- | inf @(2 * inf <= maxBound)@
  w ->
  DenseGraph w ->
  TSPResult w
runTSP inf gr = runST $ do
  dp <- UM.replicate (shiftL 1 n * n) inf

  rep n $ \v -> do
    UM.unsafeWrite dp (ixTSP (singletonBS v) v) $ matDG gr origin v

  rep (shiftL 1 n) . (. BitSet) $ \visited ->
    when (popCount visited > 1) $ do
      flip MS.mapM_ (toStreamBS visited) $ \v -> do
        MS.foldM'
          ( \acc pv -> do
              dpv <- UM.unsafeRead dp (ixTSP (deleteBS v visited) pv)
              return $ min acc (dpv + matDG gr pv v)
          )
          inf
          (n >.. 0) -- faster than (0 ..< n)
          >>= UM.unsafeWrite dp (ixTSP visited v)

  !res <-
    MS.foldM'
      ( \acc v -> do
          dv <- UM.unsafeRead dp (ixTSP visitedAll v)
          return $ min acc (dv + matDG gr v origin)
      )
      (if n > 1 then inf else 0)
      $ 0 ..< n

  TSPResult res <$> U.unsafeFreeze dp
  where
    !n = numVerticesDG gr - 1
    origin = n
    visitedAll = BitSet (shiftL 1 n - 1)

    ixTSP :: BitSet -> Int -> Int
    ixTSP visited lastPos = coerce @BitSet @Int visited * n + lastPos
    {-# INLINE ixTSP #-}
{-# INLINE runTSP #-}

{- |

/O(n^2)/

>>> let gr = fromListDG @Int [[0,1,999],[999,0,2],[4,999,0]]
>>> reconstructTSP gr $ runTSP gr
[2,0,1,2]
-}
reconstructTSP ::
  (U.Unbox w, Num w, Eq w) =>
  DenseGraph w ->
  TSPResult w ->
  U.Vector Int
reconstructTSP gr TSPResult{freezedTSP = dp, resultTSP} = U.create $ do
  path <- UM.unsafeNew (n + 2)
  UM.write path 0 origin
  UM.write path (n + 1) origin

  void $
    MS.foldM'
      ( \(!visited, !nv, !dnv) pos -> do
          let !v =
                maybe (error "reconstructTSP") id
                  . unId
                  . MS.findIndex (isPrev visited nv dnv)
                  $ 0 ..< n
          UM.write path pos v
          pure (deleteBS v visited, v, dist visited v)
      )
      (visitedAll, origin, resultTSP)
      ((n + 1) >.. 1)
  return path
  where
    !n = numVerticesDG gr - 1
    visitedAll = BitSet (shiftL 1 n - 1)
    origin = n

    isPrev visited nv dnv = \v ->
      memberBS v visited && dist visited v + matDG gr v nv == dnv
    {-# INLINE isPrev #-}

    dist visited v = U.unsafeIndex dp (coerce @BitSet visited * n + v)
    {-# INLINE dist #-}
