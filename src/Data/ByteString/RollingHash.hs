{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Data.ByteString.RollingHash where

import qualified Data.ByteString as B
import GHC.Exts
import GHC.TypeLits
import GHC.Word

import Data.RollingHash

{- | @rollingHash == rollingHashWith 2047@

>>> import qualified Data.ByteString.Char8 as C
>>> rollingHash $ C.pack "abc"
406650978
>>> rollingHashWith @2047 $ C.pack "abc"
406650978
-}
rollingHash :: B.ByteString -> RollingHash 2047
rollingHash = RollingHash . B.foldl' step 0
  where
    m# = 0x1fffffffffffffff#
    step (I# acc#) (W8# w8#) =
      case uncheckedIShiftRL# acc# 50#
        +# andI# (uncheckedIShiftL# acc# 11#) m#
        -# acc#
        +# word2Int# (word8ToWord# w8#) of
        qr# -> case qr# <# 0# of
          b# -> I# (qr# -# b# +# uncheckedIShiftL# b# 61#)

{- | @b@ should be a primitive root of (@2^61-1@)

>>> import qualified Data.ByteString.Char8 as C
>>> isPrimitiveRootRH 123
True
>>> rollingHashWith @123 $ C.pack "abc"
1479666
-}
rollingHashWith :: forall b. (KnownNat b) => B.ByteString -> RollingHash b
rollingHashWith = B.foldl' (\acc w -> acc * base + RollingHash (fromIntegral w)) 0
  where
    base = RollingHash (fromIntegral (natVal' @b proxy#))
