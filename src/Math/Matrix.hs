{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.Matrix where

import Control.Monad
import Control.Monad.ST
import Data.Primitive
import Data.Proxy
import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import Data.Vector.Fusion.Util
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Primitive as P
import GHC.Exts
import GHC.TypeLits

--
import My.Prelude (rep)

-- | n x n square matrix
data SqMat (n :: Nat) a = SqMat !Int !ByteArray

viewRowSqMat ::
  (KnownNat n, Prim a, G.Vector v a) =>
  SqMat n a ->
  Int ->
  v a
viewRowSqMat (SqMat n ba) i =
  G.unstream
    . MB.reVector
    $ MB.fromVector (P.Vector (i * n) n ba)
{-# INLINE viewRowSqMat #-}

viewColSqMat ::
  (KnownNat n, Prim a, G.Vector v a) =>
  SqMat n a ->
  Int ->
  v a
viewColSqMat (SqMat n ba) j =
  G.unstream
    . MB.map (indexByteArray ba)
    $ MB.iterateN n (+ n) j
{-# INLINE viewColSqMat #-}

createSqMat ::
  forall n a.
  (KnownNat n, Prim a) =>
  Proxy n ->
  (forall s. Int -> MutableByteArray s -> ST s ()) ->
  SqMat n a
createSqMat proxy fill = runST $ do
  let n = fromIntegral $ natVal proxy
  mba <- newByteArray (I# (sizeOf# (undefined :: a)) * n * n)
  fill n mba
  SqMat n <$!> unsafeFreezeByteArray mba
{-# INLINE createSqMat #-}

reifyMatDim :: (Integral i) => i -> (forall n. KnownNat n => Proxy n -> a) -> a
reifyMatDim n f = case someNatVal (fromIntegral n) of
  Just (SomeNat proxy) -> f proxy
  Nothing -> error $ "reifyMatDim: " <> show (fromIntegral n)
{-# INLINE reifyMatDim #-}

streamSqMat :: (Prim a, Monad m) => SqMat n a -> MS.Stream m a
streamSqMat (SqMat n ba) = MS.generateM (n * n) $ return . indexByteArray ba
{-# INLINE [1] streamSqMat #-}

unstreamSqMat :: forall n a m. (KnownNat n, Prim a) => MS.Stream Id a -> SqMat n a
unstreamSqMat s = createSqMat Proxy $ \n mba -> do
  MS.mapM_ (\(i, x) -> writeByteArray mba i x) $
    MS.trans (return . unId) $ MS.indexed s
{-# INLINE [1] unstreamSqMat #-}

{-# RULES
"streamSqMat/unstreamSqMat" forall s.
  streamSqMat (unstreamSqMat s) =
    MS.trans (return . unId) s
"unstreamSqMat/streamSqMat" forall mat.
  unstreamSqMat (streamSqMat mat) =
    mat
  #-}

liftSqMat0 :: forall n a. (KnownNat n, Num a, Prim a) => a -> SqMat n a
liftSqMat0 x = createSqMat Proxy $ \n mba -> do
  setByteArray mba 0 (n * n) (0 :: a)
  MS.mapM_ (\i -> writeByteArray mba i x) $ MS.iterateN n (+ (n + 1)) 0
{-# INLINE liftSqMat0 #-}

liftSqMat1 :: (KnownNat n, Prim a) => (a -> a) -> SqMat n a -> SqMat n a
liftSqMat1 f = unstreamSqMat . MS.map f . streamSqMat
{-# INLINE liftSqMat1 #-}

liftSqMat2 ::
  (KnownNat n, Prim a) =>
  (a -> a -> a) ->
  SqMat n a ->
  SqMat n a ->
  SqMat n a
liftSqMat2 f x y =
  unstreamSqMat $
    MS.zipWith f (streamSqMat x) (streamSqMat y)
{-# INLINE liftSqMat2 #-}

mulSqMat ::
  forall n a.
  (KnownNat n, Num a, Prim a) =>
  SqMat n a ->
  SqMat n a ->
  SqMat n a
mulSqMat x y = createSqMat Proxy $ \n mba -> do
  rep n $ \i -> do
    let r = viewRowSqMat x i
    rep n $ \j -> do
      let c = viewColSqMat y j
      writeByteArray @a mba (i * n + j) . P.sum $ P.zipWith (*) r c
{-# INLINE mulSqMat #-}

instance (KnownNat n, Num a, Prim a) => Num (SqMat n a) where
  {-# SPECIALIZE instance (KnownNat n) => Num (SqMat n Int) #-}
  {-# SPECIALIZE instance (KnownNat n) => Num (SqMat n Double) #-}
  (+) = liftSqMat2 (+)
  {-# INLINE (+) #-}
  (-) = liftSqMat2 (-)
  {-# INLINE (-) #-}
  (*) = mulSqMat
  {-# INLINE (*) #-}
  negate = liftSqMat1 negate
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = id
  {-# INLINE signum #-}
  fromInteger = liftSqMat0 . fromInteger
  {-# INLINE fromInteger #-}
