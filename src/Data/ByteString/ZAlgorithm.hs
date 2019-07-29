{-# LANGUAGE BangPatterns #-}

module Data.ByteString.ZAlgorithm where

import           Control.Monad.Primitive
import qualified Data.ByteString             as B
import qualified Data.ByteString.Unsafe      as B
import           Data.Coerce                 (coerce)
import           Data.Function               (fix)
import qualified Data.Primitive.ByteArray    as BA
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Foreign.Storable            (sizeOf)

-- | z[i] = lcp s $ drop i s
-- time complexity: O(n)
-- >>> zAlgorithm "ababab"
-- [6,0,4,0,2,0]
-- >>> zAlgorithm "abc$xabcxx"
-- [10,0,0,0,0,3,0,0,0,0]
zAlgorithm :: B.ByteString -> U.Vector Int
zAlgorithm bs
    | B.null bs = U.empty
    | otherwise = U.create $ do
        let n = B.length bs
        z <- UM.replicate n 0
        UM.write z 0 n
        zbox <- newZBox
        U.forM_ (U.tail $ U.generate n id) $ \i -> do
            r <- readR zbox
            if i > r
            then do
                writeL zbox i
                writeR zbox i
                extendR bs zbox >>= UM.unsafeWrite z i
            else do
                l <- readL zbox
                let k = i - l
                zk <- UM.unsafeRead z k
                if zk < r - i + 1
                then do
                    UM.unsafeWrite z i zk
                else do
                    writeL zbox i
                    extendR bs zbox >>= UM.unsafeWrite z i
        return z

extendR :: (PrimMonad m) => B.ByteString -> ZBox (PrimState m) -> m Int
extendR bs zbox = do
    l <- readL zbox
    r <- readR zbox
    let !n = B.length bs
    let r' = flip fix r $ \loop !i ->
            if i < n && B.unsafeIndex bs (i - l) == B.unsafeIndex bs i
            then loop (i + 1)
            else i
    writeR zbox (r' - 1)
    return $! r' - l
{-# INLINE extendR #-}

newtype ZBox s = ZBox { unZBox :: BA.MutableByteArray s }

newZBox :: (PrimMonad m) => m (ZBox (PrimState m))
newZBox = do
    zbox <- ZBox <$> BA.newByteArray (sizeOf (undefined :: Int) * 2)
    writeL zbox 0
    writeR zbox 0
    return zbox

readL :: (PrimMonad m) => ZBox (PrimState m) -> m Int
readL zbox = BA.readByteArray (coerce zbox) 0
{-# INLINE readL #-}

readR:: (PrimMonad m) => ZBox (PrimState m) -> m Int
readR zbox = BA.readByteArray (coerce zbox) 1
{-# INLINE readR #-}

writeL :: (PrimMonad m) => ZBox (PrimState m) -> Int -> m ()
writeL zbox = BA.writeByteArray (coerce zbox) 0
{-# INLINE writeL #-}

writeR :: (PrimMonad m) => ZBox (PrimState m) -> Int -> m ()
writeR zbox = BA.writeByteArray (coerce zbox) 1
{-# INLINE writeR #-}
