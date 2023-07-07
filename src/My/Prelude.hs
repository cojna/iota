{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

module My.Prelude where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Bits
import Data.Bool
import qualified Data.ByteString.Builder as B
import qualified Data.Foldable as F
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import qualified Data.Vector.Fusion.Bundle.Size as Bundle
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import Data.Vector.Fusion.Util
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts
import System.IO

import Data.PrimParser

-- * Stream utils
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (0 ..< n)
{-# INLINE rep #-}

rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (1 ..< n + 1)
{-# INLINE rep1 #-}

rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (n >.. 0)
{-# INLINE rev #-}

rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (n + 1 >.. 1)
{-# INLINE rev1 #-}

infix 4 ..<

{- |
>>> runIdentity $ MS.toList (0..<5)
[0,1,2,3,4]
-}
(..<) :: (Monad m) => Int -> Int -> MS.Stream m Int
(..<) !l !r = MS.Stream step l
  where
    step x
      | x < r = return $ MS.Yield x (x + 1)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] (..<) #-}

infix 4 >..

{- |
>>> runIdentity $ MS.toList (5>..0)
[4,3,2,1,0]
-}
(>..) :: (Monad m) => Int -> Int -> MS.Stream m Int
(>..) !r !l = MS.Stream step (r - 1)
  where
    step x
      | x >= l = return $ MS.Yield x (x - 1)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] (>..) #-}

{- |
>>> runIdentity $ MS.toList (stride 0 10 3)
[0,3,6,9]
-}
stride :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stride !l !r !d = MS.Stream step l
  where
    step x
      | x < r = return $ MS.Yield x (x + d)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stride #-}

liftMS :: (Monad m) => MS.Stream Id a -> MS.Stream m a
liftMS = MS.trans (return . unId)
{-# INLINE liftMS #-}

-- * Vector utils

asUVector :: U.Vector a -> U.Vector a
asUVector = id
{-# INLINE asUVector #-}

asBVector :: V.Vector a -> V.Vector a
asBVector = id
{-# INLINE asBVector #-}

{- |
>>> lowerBound (U.fromList "122333") '2'
1
>>> lowerBound (U.fromList "122333") '0'
0
>>> lowerBound (U.fromList "122333") '9'
6
-}
lowerBound :: (Ord a, G.Vector v a) => v a -> a -> Int
lowerBound !vec !key = binarySearch 0 (G.length vec) ((key <=) . G.unsafeIndex vec)
{-# INLINE lowerBound #-}

{- |
>>> upperBound (U.fromList "122333") '2'
3
>>> upperBound (U.fromList "122333") '0'
0
>>> upperBound (U.fromList "122333") '9'
6
-}
upperBound :: (Ord a, G.Vector v a) => v a -> a -> Int
upperBound !vec !key = binarySearch 0 (G.length vec) ((key <) . G.unsafeIndex vec)
{-# INLINE upperBound #-}

radixSort :: U.Vector Int -> U.Vector Int
radixSort v0 = F.foldl' step v0 [0, 16, 32, 48]
  where
    mask k x = unsafeShiftRL x k .&. 0xffff
    step v k = U.create $ do
      pos <- UM.unsafeNew 0x10001
      UM.set pos 0
      U.forM_ v $ \x -> do
        UM.unsafeModify pos (+ 1) (mask k x + 1)
      rep 0xffff $ \i -> do
        fi <- UM.unsafeRead pos i
        UM.unsafeModify pos (+ fi) (i + 1)
      res <- UM.unsafeNew $ U.length v
      U.forM_ v $ \x -> do
        let !masked = mask k x
        i <- UM.unsafeRead pos masked
        UM.unsafeWrite pos masked $ i + 1
        UM.unsafeWrite res i x
      return res
{-# INLINE radixSort #-}

{- |
>>> runLengthEncode $ U.fromList "abbccc"
[('a',1),('b',2),('c',3)]
>>> runLengthEncode $ U.fromList ""
[]
-}
runLengthEncode ::
  (Eq a, G.Vector v a, G.Vector v (a, Int)) =>
  v a ->
  v (a, Int)
runLengthEncode =
  G.unstream
    . Bundle.inplace streamRLE Bundle.toMax
    . G.stream
{-# INLINE runLengthEncode #-}

streamRLE :: (Eq a, Monad m) => MS.Stream m a -> MS.Stream m (a, Int)
streamRLE (MS.Stream step s0) = MS.Stream step' (Nothing, s0)
  where
    step' (Nothing, s) = do
      r <- step s
      case r of
        MS.Yield x s' -> return $ MS.Skip (Just (x, 1), s')
        MS.Skip s' -> return $ MS.Skip (Nothing, s')
        MS.Done -> return MS.Done
    step' (Just (x, !i), s) = do
      r <- step s
      case r of
        MS.Yield y s'
          | x == y -> return $ MS.Skip (Just (x, i + 1), s')
          | otherwise -> return $ MS.Yield (x, i) (Just (y, 1), s')
        MS.Skip s' -> return $ MS.Skip (Just (x, i), s')
        MS.Done -> return $ MS.Yield (x, i) (Nothing, s)
    {-# INLINE [0] step' #-}
{-# INLINE [1] streamRLE #-}

{- |
>>> forAccum (0 :: Int) (U.fromList "abc") $ \acc c -> (acc + 1, (acc, c))
[(0,'a'),(1,'b'),(2,'c')]
-}
forAccum ::
  (G.Vector v a, G.Vector v b) =>
  s ->
  v a ->
  (s -> a -> (s, b)) ->
  v b
forAccum x v f = mapAccum f x v
{-# INLINE forAccum #-}

mapAccum ::
  (G.Vector v a, G.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  v b
mapAccum f x =
  G.unstream
    . Bundle.inplace
      (streamAccumM (\s a -> pure (f s a)) x)
      Bundle.toMax
    . G.stream
{-# INLINE mapAccum #-}

forAccumM ::
  (PrimMonad m, G.Vector v a, G.Vector v b) =>
  s ->
  v a ->
  (s -> a -> m (s, b)) ->
  m (v b)
forAccumM s v f = mapAccumM f s v
{-# INLINE forAccumM #-}

forAccumM_ ::
  (Monad m, G.Vector v b) =>
  a ->
  v b ->
  (a -> b -> m a) ->
  m ()
forAccumM_ x v f = void $ G.foldM'_ f x v
{-# INLINE forAccumM_ #-}

mapAccumM ::
  (PrimMonad m, G.Vector v a, G.Vector v b) =>
  (s -> a -> m (s, b)) ->
  s ->
  v a ->
  m (v b)
mapAccumM f x =
  (>>= G.unsafeFreeze)
    . GM.munstream
    . bundleAccumM f x
    . Bundle.lift
    . G.stream
{-# INLINE mapAccumM #-}

bundleAccumM ::
  (Monad m) =>
  (s -> a -> m (s, b)) ->
  s ->
  MBundle.Bundle m v a ->
  MBundle.Bundle m v b
bundleAccumM f x bundle =
  MBundle.fromStream
    (streamAccumM f x (MBundle.elements bundle))
    (MBundle.size bundle)
{-# INLINE [1] bundleAccumM #-}

streamAccumM :: (Monad m) => (s -> a -> m (s, b)) -> s -> MS.Stream m a -> MS.Stream m b
streamAccumM f s0 (MS.Stream step x0) = MS.Stream step' (s0, x0)
  where
    step' (!s, x) = do
      r <- step x
      case r of
        MS.Yield a x' -> do
          (s', b) <- f s a
          return $ MS.Yield b (s', x')
        MS.Skip x' -> return $ MS.Skip (s, x')
        MS.Done -> return MS.Done
    {-# INLINE [0] step' #-}
{-# INLINE [1] streamAccumM #-}

stream :: (G.Vector v a) => v a -> MS.Stream Id a
stream = MBundle.elements . G.stream
{-# INLINE stream #-}

streamM :: (G.Vector v a, Monad m) => v a -> MS.Stream m a
streamM = MS.trans (return . unId) . MBundle.elements . G.stream
{-# INLINE streamM #-}

{- |
>>> asUVector . unstream 10 . stream $ U.fromList "abc"
"abc"
-}
unstream :: (G.Vector v a) => Int -> MS.Stream Id a -> v a
unstream ub =
  G.unstream
    . flip MBundle.fromStream (Bundle.Max ub)
{-# INLINE unstream #-}

unstreamM ::
  (PrimMonad m, G.Vector v a) =>
  Int ->
  MS.Stream m a ->
  m (v a)
unstreamM ub s =
  GM.munstream
    (MBundle.fromStream s (Bundle.Max ub))
    >>= G.unsafeFreeze
{-# INLINE [1] unstreamM #-}

-- * Bits utils
infixl 8 `shiftRL`, `unsafeShiftRL`, !>>>.

shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}

{- |
>>> unsafeShiftR (-1) 1
-1
>>> unsafeShiftRL (-1) 1
9223372036854775807
-}
unsafeShiftRL :: Int -> Int -> Int
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}

(!>>>.) :: Int -> Int -> Int
(!>>>.) = unsafeShiftRL
{-# INLINE (!>>>.) #-}

-- * Parser utils
uvectorN :: (U.Unbox a) => Int -> PrimParser a -> PrimParser (U.Vector a)
uvectorN = gvectorN
{-# INLINE uvectorN #-}

bvectorN :: Int -> PrimParser a -> PrimParser (V.Vector a)
bvectorN = gvectorN
{-# INLINE bvectorN #-}

gvectorN :: (G.Vector v a) => Int -> PrimParser a -> PrimParser (v a)
gvectorN n f = do
  (e, o) <- viewPrimParser
  pure $ G.unfoldrN n (pure . runPrimParser f e) o
{-# INLINE gvectorN #-}

streamN :: Int -> PrimParser a -> PrimParser (MS.Stream Id a)
streamN n f = do
  (e, o) <- viewPrimParser
  pure $ MS.unfoldrN n (pure . runPrimParser f e) o
{-# INLINE streamN #-}

uvector :: (U.Unbox a) => PrimParser a -> PrimParser (U.Vector a)
uvector = gvector
{-# INLINE uvector #-}

gvector :: (G.Vector v a) => PrimParser a -> PrimParser (v a)
gvector f = do
  (e, o) <- viewPrimParser
  pure $
    G.unfoldr
      ( \p -> case runPrimParser f e p of
          (x, p')
            | p' < e -> Just (x, p')
            | otherwise -> Nothing
      )
      o
{-# INLINE gvector #-}

-- * Builder utils
unlinesB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unlinesB f = G.foldr' ((<>) . (<> endlB) . f) mempty

unwordsB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unwordsB f vec
  | G.null vec = mempty
  | otherwise =
      f (G.head vec)
        <> G.foldr' ((<>) . (B.char7 ' ' <>) . f) mempty (G.tail vec)

concatB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
concatB f = G.foldr ((<>) . f) mempty

{- |
>>> B.toLazyByteString . matrixB 2 3 B.intDec $ U.fromListN 6 [1, 2, 3, 4, 5, 6]
"1 2 3\n4 5 6\n"
-}
matrixB :: (G.Vector v a) => Int -> Int -> (a -> B.Builder) -> v a -> B.Builder
matrixB h w f mat =
  F.foldMap
    ((<> endlB) . unwordsB f)
    [G.slice (i * w) w mat | i <- [0 .. h - 1]]

{- |
>>> B.toLazyByteString . gridB 2 3 B.char7 $ U.fromListN 6 ".#.#.#"
".#.\n#.#\n"
>>> B.toLazyByteString . gridB 2 3 B.intDec $ U.fromListN 6 [1, 2, 3, 4, 5, 6]
"123\n456\n"
-}
gridB :: (G.Vector v a) => Int -> Int -> (a -> B.Builder) -> v a -> B.Builder
gridB h w f mat =
  F.foldMap
    ((<> endlB) . concatB f)
    [G.slice (i * w) w mat | i <- [0 .. h - 1]]

sizedB :: (G.Vector v a) => (v a -> B.Builder) -> v a -> B.Builder
sizedB f vec = B.intDec (G.length vec) <> endlB <> f vec

yesnoB :: Bool -> B.Builder
yesnoB = bool (B.string7 "No") (B.string7 "Yes")

{- |
>>> B.toLazyByteString . pairB B.intDec B.intDec $ (0, 1)
"0 1"
>>> B.toLazyByteString . pairB B.intDec (pairB B.intDec B.intDec) $ (0, (1, 2))
"0 1 2"
-}
pairB :: (a -> B.Builder) -> (b -> B.Builder) -> (a, b) -> B.Builder
pairB f g (x, y) = f x <> B.char7 ' ' <> g y

showB :: (Show a) => a -> B.Builder
showB = B.string7 . show

showLnB :: (Show a) => a -> B.Builder
showLnB = B.string7 . flip shows "\n"

endlB :: B.Builder
endlB = B.char7 '\n'
{-# INLINE endlB #-}

putBuilder :: (MonadIO m) => B.Builder -> m ()
putBuilder = liftIO . B.hPutBuilder stdout

putBuilderLn :: (MonadIO m) => B.Builder -> m ()
putBuilderLn b = putBuilder b *> putBuilder (B.char7 '\n')

-- * Misc
neighbor4 :: (Applicative f) => Int -> Int -> Int -> (Int -> f ()) -> f ()
neighbor4 h w xy f =
  when (x /= 0) (f $ xy - w)
    *> when (y /= 0) (f $ xy - 1)
    *> when (y /= w - 1) (f $ xy + 1)
    *> when (x /= h - 1) (f $ xy + w)
  where
    (!x, !y) = quotRem xy w
{-# INLINE neighbor4 #-}

binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low0 high0 p = go low0 high0
  where
    go !low !high
      | high <= low = return high
      | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid)
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE binarySearchM #-}

binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity $ binarySearchM low high (return . p)
{-# INLINE binarySearch #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
  let !x = unsafeShiftRL xy 32
      !y = xy .&. 0xffffffff
   in (x, y)
{-# INLINE decode32x2 #-}

runSolver :: (a -> IO ()) -> PrimParser a -> IO ()
runSolver = withInputHandle stdin
