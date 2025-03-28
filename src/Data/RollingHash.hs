{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.RollingHash where

import Data.Char
import qualified Data.Foldable as F
import Data.Proxy
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import GHC.Real (Ratio (..))
import GHC.TypeLits
import System.Random.Stateful

import My.Prelude

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

instance (KnownNat b) => Fractional (RollingHash b) where
  recip = (^ (0x1ffffffffffffffd :: Int))
  {-# INLINE recip #-}
  fromRational (p :% q) = fromInteger p / fromInteger q

{- |
>>> fromString @(RollingHash 2047) "abc"
406650978
>>> fromString @(RollingHash 2047) "cba"
415031394
>>> fromString @(RollingHash 100) "abc"
979899
-}
instance (KnownNat b) => IsString (RollingHash b) where
  fromString = F.foldl' snocRH emptyRH

newtype instance U.MVector s (RollingHash b) = MV_RollingHash (U.MVector s Int)
newtype instance U.Vector (RollingHash b) = V_RollingHash (U.Vector Int)
deriving newtype instance GM.MVector U.MVector (RollingHash b)
deriving newtype instance G.Vector U.Vector (RollingHash b)
instance U.Unbox (RollingHash b)

data RollingHashTable (b :: Nat) = RollingHashTable
  { originalRH :: !(U.Vector Char)
  , hashForwardRH :: !(U.Vector (RollingHash b))
  , hashReverseRH :: !(U.Vector (RollingHash b))
  , powCacheRH :: !(U.Vector (RollingHash b))
  , recipPowCacheRH :: !(U.Vector (RollingHash b))
  }

buildRollingHashTable ::
  forall (b :: Nat).
  (KnownNat b) =>
  U.Vector Char ->
  RollingHashTable b
buildRollingHashTable originalRH =
  RollingHashTable
    { originalRH
    , hashForwardRH
    , hashReverseRH
    , powCacheRH
    , recipPowCacheRH
    }
  where
    !b = RollingHash (fromIntegral (natVal' @b proxy#))
    !b' = recip b
    hashForwardRH = U.scanl' (\h c -> h * b + RollingHash (ord c)) 0 originalRH
    (hashReverseRH, powCacheRH) =
      U.unzip $
        U.scanl'
          ( \(!h, !powb) c ->
              (RollingHash (ord c) * powb + h, powb * b)
          )
          (0, 1)
          originalRH
    recipPowCacheRH = U.scanl' (\powb' _ -> powb' * b') 1 originalRH

{- |
>>> rht = buildRollingHashTable @2047 $ U.fromList "abcdef"
>>> rollingHashTo rht 3
406650978
>>> fromString @(RollingHash 2047) "abc"
406650978
-}
rollingHashTo :: (KnownNat b) => RollingHashTable b -> Int -> RollingHash b
rollingHashTo RollingHashTable{hashForwardRH} i = U.unsafeIndex hashForwardRH i

{- |
>>> rht = buildRollingHashTable @2047 $ U.fromList "abcdef"
>>> rollingHashFromTo rht 1 4
410843235
>>> fromString @(RollingHash 2047) "bcd"
410843235
-}
rollingHashFromTo ::
  (KnownNat b) =>
  RollingHashTable b ->
  Int ->
  Int ->
  RollingHash b
rollingHashFromTo RollingHashTable{hashForwardRH, powCacheRH} l r =
  hr - hl * brl
  where
    hl = U.unsafeIndex hashForwardRH l
    hr = U.unsafeIndex hashForwardRH r
    brl = U.unsafeIndex powCacheRH (r - l)

{- |
>>> rht = buildRollingHashTable @2047 $ U.fromList "abcdef"
>>> rollingHashReverseTo rht 3
415031394
>>> fromString @(RollingHash 2047) "cba"
415031394
-}
rollingHashReverseTo :: (KnownNat b) => RollingHashTable b -> Int -> RollingHash b
rollingHashReverseTo RollingHashTable{hashReverseRH} i = U.unsafeIndex hashReverseRH i

{- |
>>> rht = buildRollingHashTable @2047 $ U.fromList "abcdef"
>>> rollingHashReverseFromTo rht 1 4
419223651
>>> fromString @(RollingHash 2047) "dcb"
419223651
-}
rollingHashReverseFromTo ::
  (KnownNat b) =>
  RollingHashTable b ->
  Int ->
  Int ->
  RollingHash b
rollingHashReverseFromTo RollingHashTable{hashReverseRH, recipPowCacheRH} l r =
  (hr - hl) * bl'
  where
    hl = U.unsafeIndex hashReverseRH l
    hr = U.unsafeIndex hashReverseRH r
    bl' = U.unsafeIndex recipPowCacheRH l

{- |
/O(1)/

>>> rht = buildRollingHashTable @2047 $ U.fromList "mississippi"
>>> isPalindromeFromTo rht 0 11
False
>>> isPalindromeFromTo rht 1 5
True
>>> isPalindromeFromTo rht 1 8
True
>>> isPalindromeFromTo rht 0 0
True
>>> isPalindromeFromTo rht 11 11
True
-}
isPalindromeFromTo :: (KnownNat b) => RollingHashTable b -> Int -> Int -> Bool
isPalindromeFromTo rht l r =
  rollingHashFromTo rht l r == rollingHashReverseFromTo rht l r

{- |
>>> cacheSize = 10
>>> rht = buildRollingHashTable @2047 $ U.replicate cacheSize 'x'
>>> ab = "ab"
>>> cde = "cde"
>>> appendRH rht (length ab) (fromString ab) (length cde) (fromString cde)
1703952588079203
>>> fromString @(RollingHash 2047) "abcde"
1703952588079203

>>> rht = buildRollingHashTable @2047 U.empty
>>> ab = "ab"
>>> cde = "cde"
>>> appendRH rht (length ab) (fromString ab) (length cde) (fromString cde)
1703952588079203
>>> fromString @(RollingHash 2047) "abcde"
1703952588079203
-}
appendRH ::
  forall (b :: Nat).
  (KnownNat b) =>
  RollingHashTable b ->
  -- | length
  Int ->
  RollingHash b ->
  -- | length
  Int ->
  RollingHash b ->
  RollingHash b
appendRH RollingHashTable{powCacheRH} _lx hx ly hy
  | ly < U.length powCacheRH = hx * U.unsafeIndex powCacheRH ly + hy
  | otherwise =
      let !b = RollingHash (fromIntegral (natVal' @b proxy#))
       in hx * (b ^ ly) + hy

{- |
LCP(Longet Common Prefix) of @s[i..n)@ and @s[j..n)@

/O(log n)/

>>> rht = buildRollingHashTable @2047 $ U.fromList "abcxxxabc"
>>> longestCommonPrefixRH rht 0 6
3
>>> longestCommonPrefixRH rht 0 0
9
>>> longestCommonPrefixRH rht 8 8
1
>>> longestCommonPrefixRH rht 0 1
0
-}
longestCommonPrefixRH :: (KnownNat b) => RollingHashTable b -> Int -> Int -> Int
longestCommonPrefixRH rht i j | i > j = longestCommonPrefixRH rht i j
longestCommonPrefixRH rht@RollingHashTable{originalRH} i j =
  binarySearch 0 (n - j) $ \k ->
    rollingHashFromTo rht i (i + k + 1) /= rollingHashFromTo rht j (j + k + 1)
  where
    !n = U.length originalRH

{- |
/O(log n)/

>>> import Data.List
>>> rht = buildRollingHashTable @2047 $ U.fromList "mississippi"
>>> sortBy (compareSuffixRH rht) [0..11]
[11,10,7,4,1,0,9,8,6,3,5,2]
>>> rht = buildRollingHashTable @2047 $ U.fromList "abracadabra"
>>> sortBy (compareSuffixRH rht) [0..11]
[11,10,7,0,3,5,8,1,4,6,9,2]
>>> rht = buildRollingHashTable @2047 $ U.fromList "ababab"
>>> sortBy (compareSuffixRH rht) [0..6]
[6,4,2,0,5,3,1]
-}
compareSuffixRH ::
  (KnownNat b) =>
  RollingHashTable b ->
  Int ->
  Int ->
  Ordering
compareSuffixRH !rht@RollingHashTable{originalRH} i j
  | i + k < n
  , j + k < n =
      compare
        (U.unsafeIndex originalRH (i + k))
        (U.unsafeIndex originalRH (j + k))
  | otherwise = compare j i
  where
    !k = longestCommonPrefixRH rht i j
    !n = U.length originalRH

{- |
Rabin–Karp

/O(n)/

>>> rht = buildRollingHashTable @2047 $ U.fromList "xxxabcxxx"
>>> abc = "abc"
>>> findSubstringIndexRH rht (length abc) (fromString abc)
Just 3
>>> zzz = "zzz"
>>> findSubstringIndexRH rht (length zzz) (fromString zzz)
Nothing
-}
findSubstringIndexRH ::
  (KnownNat b) =>
  RollingHashTable b ->
  -- | length
  Int ->
  RollingHash b ->
  Maybe Int
findSubstringIndexRH
  RollingHashTable
    { hashForwardRH
    , powCacheRH
    }
  !len
  !hash =
    U.findIndex (== hash) $
      U.zipWith (\hr hl -> hr - hl * b) (U.drop len hashForwardRH) hashForwardRH
    where
      !b = U.unsafeIndex powCacheRH len

{- |
Rabin–Karp

/O(n)/

>>> rht = buildRollingHashTable @2047 $ U.fromList "xxxabcxxx"
>>> abc = "abc"
>>> findSubstringIndicesRH rht (length abc) (fromString abc)
[3]
>>> xxx = "xxx"
>>> findSubstringIndicesRH rht (length xxx) (fromString xxx)
[0,6]
>>> zzz = "zzz"
>>> findSubstringIndicesRH rht (length zzz) (fromString zzz)
[]
-}
findSubstringIndicesRH ::
  (KnownNat b) =>
  RollingHashTable b ->
  Int ->
  RollingHash b ->
  U.Vector Int
findSubstringIndicesRH
  RollingHashTable
    { hashForwardRH
    , powCacheRH
    }
  !len
  !hash =
    U.findIndices (== hash) $
      U.zipWith (\hr hl -> hr - hl * b) (U.drop len hashForwardRH) hashForwardRH
    where
      !b = U.unsafeIndex powCacheRH len

asRollingHashOf :: RollingHash b -> Proxy b -> RollingHash b
asRollingHashOf = const

emptyRH :: RollingHash b
emptyRH = RollingHash 0

singletonRH :: Int -> RollingHash b
singletonRH = RollingHash

{- |
>>> F.foldl' (snocRH @2047) emptyRH "abc"
406650978
-}
snocRH ::
  forall (b :: Nat).
  (KnownNat b) =>
  RollingHash b ->
  Char ->
  RollingHash b
snocRH h c = h * base + RollingHash (ord c)
  where
    base = RollingHash (fromIntegral (natVal' @b proxy#))

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
