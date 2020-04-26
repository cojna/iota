{-# LANGUAGE BangPatterns, CPP, LambdaCase, OverloadedLists, ViewPatterns #-}

module Data.SegTreeSpec (main, spec) where

import           Data.Bits
import           Data.SegTree
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
import           Data.Semigroup              as Semigroup
#endif
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.Exts
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "runSegTree" $ do
        prop "naive" $ prop_runNaiveSegTree
        it "minimum [0, 5) for [0..4]" $
            runSegTree [0..4] [SegQuery 0 5] `shouldBe` U.fromList [0 :: Int]
        it "minimum [3, 4) for [0..4]" $
            runSegTree [0..4] [SegQuery 3 4] `shouldBe` U.fromList [3 :: Int]
        it "minimum [3, 4) for update 3 (-1) [0..4]" $
            runSegTree [0..4] [SegUpdate 3 (-1), SegQuery 3 4]
                `shouldBe` U.fromList [(-1) :: Int]
        it "minimum [0, 5) for update 3 (-1) [0..4]" $
            runSegTree [0..4] [SegUpdate 3 (-1), SegQuery 0 5]
                `shouldBe` U.fromList [(-1) :: Int]
        it "minimum [0, 1) for [0]" $
            runSegTree [0] [SegQuery 0 1]
                `shouldBe` U.fromList [0 :: Int]
        it "minimum [0, 1) for update 0 (-1) [0]" $
            runSegTree [0] [SegUpdate 0 (-1), SegQuery 0 1]
                `shouldBe` U.fromList [(-1) :: Int]
    describe "extendToPowerOfTwo" $ do
        it "f 0 == 1" $ extendToPowerOfTwo 0 `shouldBe` 1
        it "f 1 == 1" $ extendToPowerOfTwo 1 `shouldBe` 1
        it "f 2 == 2" $ extendToPowerOfTwo 2 `shouldBe` 2
        it "f 3 == 4" $ extendToPowerOfTwo 3 `shouldBe` 4
        prop "popCount (f x) == 1" $ prop_powerOfTwo
        prop "x <= f x" $ prop_greaterThanOrEqual
        prop "popCount (f x / 2) /= 1 || f x / 2 < x" $ prop_least

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
instance Semigroup.Semigroup Int where
  (<>) = min
#endif

instance Monoid Int where
    mempty = maxBound
    {-# INLINE mempty #-}
#if MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)
#elif MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = min
    {-# INLINE mappend #-}
#endif

data SegTreeQuery a
    = SegUpdate !Int !a
    | SegQuery !Int !Int

runSegTree
    :: (Monoid a, U.Unbox a)
    => U.Vector a -> V.Vector (SegTreeQuery a) -> U.Vector a
runSegTree vec queries = U.create $ do
    seg <- buildSegTree vec
    res <- UM.replicate (V.length queries) mempty
    size <- V.foldM' (\acc -> \case
            SegUpdate k v -> do
                writeSegTree seg k v
                return acc
            SegQuery l r -> do
                mappendFromTo seg l r >>= UM.unsafeWrite res acc
                return $ acc + 1
        ) 0 queries
    return $ UM.take size res


runNaiveSegTree
    :: (Monoid a, U.Unbox a)
    => U.Vector a -> V.Vector (SegTreeQuery a) -> U.Vector a
runNaiveSegTree vec queries = U.create $ do
    seg <- U.thaw vec
    res <- UM.replicate (V.length queries) mempty
    size <- V.foldM' (\acc -> \case
            SegUpdate k v -> do
                UM.write seg k v
                return acc
            SegQuery l r -> do
                let go !i !m
                        | i < r = UM.read seg i >>= go (i + 1) . mappend m
                        | otherwise = UM.unsafeWrite res acc m
                go l mempty
                return $ acc + 1
        ) 0 queries
    return $ UM.take size res

prop_runNaiveSegTree :: NonEmptyList Int -> [(Bool, Int, Int)] -> Bool
prop_runNaiveSegTree (getNonEmpty -> xs) qs
    = runSegTree vec queries == runNaiveSegTree vec queries
  where
    vec = U.fromList xs
    len = U.length vec
    ix i = mod i len
    queries = V.fromList . flip map qs $ \case
        (True, k, v) -> SegUpdate (ix k) v
        (False, l, r) -> SegQuery li (min (li + 1) ri)
          where
            li = ix l `min` ix l
            ri = ix l `min` ix r + 1

prop_powerOfTwo :: NonNegative Int -> Bool
prop_powerOfTwo (getNonNegative -> x)
    = popCount (extendToPowerOfTwo x) == 1

prop_greaterThanOrEqual :: NonNegative Int -> Bool
prop_greaterThanOrEqual (getNonNegative -> x) = x <= extendToPowerOfTwo x

prop_least :: NonNegative Int -> Bool
prop_least (getNonNegative -> x) = not $ x <= prev && popCount prev == 1
  where
    res = extendToPowerOfTwo x
    prev = res `shiftR` 1
