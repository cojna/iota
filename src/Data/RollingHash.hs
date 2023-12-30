{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.RollingHash where

import Data.Proxy
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import GHC.TypeLits
import System.Random.Stateful

{- |
modulo @2^61-1@

@b@ should be a primitive root of @2^61-1@
-}
newtype RollingHash (b :: Nat) = RollingHash {getRollingHash :: Int}
  deriving newtype (Eq, Ord, Show)

instance Num (RollingHash b) where
  (RollingHash (I# x#)) + (RollingHash (I# y#)) = case x# +# y# of
    xy# -> case xy# >=# 0x1fffffffffffffff# of
      b# -> RollingHash (I# (xy# +# b# -# uncheckedIShiftL# b# 61#))
  {-# INLINE (+) #-}
  (RollingHash (I# x#)) - (RollingHash (I# y#)) = case x# -# y# of
    xy# -> case xy# <# 0# of
      b# -> RollingHash (I# (xy# -# b# +# uncheckedIShiftL# b# 61#))
  {-# INLINE (-) #-}
  (RollingHash (I# x#)) * (RollingHash (I# y#)) = case timesWord2# (int2Word# x#) (int2Word# y#) of
    (# hi#, lo# #) ->
      case uncheckedIShiftL# (word2Int# hi#) 3#
        +# uncheckedIShiftRL# (word2Int# lo#) 61#
        +# andI# (word2Int# lo#) 0x1fffffffffffffff# of
        qr# -> case qr# ># 0x1fffffffffffffff# of
          b# -> RollingHash (I# (qr# +# b# -# uncheckedIShiftL# b# 61#))
  {-# INLINE (*) #-}
  abs = id
  {-# INLINE abs #-}
  signum = const (RollingHash 1)
  {-# INLINE signum #-}
  fromInteger x = RollingHash $ fromIntegral (mod x 0x1fffffffffffffff)
  {-# INLINE fromInteger #-}

newtype instance U.MVector s (RollingHash b) = MV_RollingHash (U.MVector s Int)
newtype instance U.Vector (RollingHash b) = V_RollingHash (U.Vector Int)
deriving newtype instance GM.MVector U.MVector (RollingHash b)
deriving newtype instance G.Vector U.Vector (RollingHash b)
instance U.Unbox (RollingHash b)

asRollingHashOf :: RollingHash b -> Proxy b -> RollingHash b
asRollingHashOf = const

{- |
check if @g@ is a primitive root of @2^61-1@.

>>> isPrimitiveRootRH 2047
True
>>> take 10 $ filter isPrimitiveRootRH [2..]
[37,43,55,69,74,86,110,123,133,138]
-}
isPrimitiveRootRH :: Int -> Bool
isPrimitiveRootRH g = all ok [2, 3, 5, 7, 11, 13, 31, 41, 61, 151, 331, 1321]
  where
    ok :: Int -> Bool
    ok p = fromIntegral g ^ quot 0x1ffffffffffffffe p /= (1 :: RollingHash 2047)

{- |
Generate a primitive root of @2^61-1@.

>>> genPrimitiveRootRH (mkStdGen 123)
1600615663002506808
>>> isPrimitiveRootRH 1600615663002506808
True
-}
genPrimitiveRootRH :: (RandomGen g) => g -> Int
genPrimitiveRootRH = go
  where
    go !g
      | isPrimitiveRootRH x = x
      | otherwise = go g'
      where
        (x, g') = randomR (2, 0x1ffffffffffffffe) g

{- |
>>> withPrimitiveRootRH (mkStdGen 123) $ \proxy -> getRollingHash $ 456 `asRollingHashOf` proxy
456
-}
withPrimitiveRootRH ::
  (RandomGen g) =>
  g ->
  (forall b. (KnownNat b) => Proxy b -> a) ->
  a
withPrimitiveRootRH gen f =
  case someNatVal (fromIntegral $ genPrimitiveRootRH gen) of
    Just (SomeNat proxy) -> f proxy
    Nothing -> error "withPrimitiveRootRH failed"
{-# INLINE withPrimitiveRootRH #-}
