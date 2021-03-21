{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SegTree.GenericSpec (main, spec) where

import Data.Bits
import Data.Monoid
import Data.SegTree.Generic
import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts
import Test.Prelude

import Data.Vector.Unboxed.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildSegTree" $ do
    prop "naive" prop_buildSegTree
  describe "readSegTree/writeSegTree" $ do
    it "{write 0 v; read 0} == v" $ do
      let v = 123
      seg <- buildSegTree @(Min Int) $ U.fromList [10 .. 15]
      writeSegTree seg 0 v
      res <- readSegTree seg 0
      res `shouldBe` v
    it "{write (n-1) v; read (n-1)} == v" $ do
      let n = 4
      let v = 123
      seg <- buildSegTree @(Min Int) $ U.fromList [10 .. 13]
      writeSegTree seg (n - 1) v
      res <- readSegTree seg (n - 1)
      res `shouldBe` v
  describe "readSegTree/modifySegTree" $ do
    it "{modify (const v) 0; read 0} == v" $ do
      let v = 123
      seg <- buildSegTree @(Min Int) $ U.fromList [10 .. 15]
      modifySegTree seg (const v) 0
      res <- readSegTree seg 0
      res `shouldBe` v
    it "{modify (const v) (n-1); read (n-1)} == v" $ do
      let n = 4
      let v = 123
      seg <- buildSegTree @(Min Int) $ U.fromList [10 .. 13]
      modifySegTree seg (const v) (n - 1)
      res <- readSegTree seg (n - 1)
      res `shouldBe` v
  describe "runSegTree" $ do
    prop "naive" prop_runNaiveSegTree
    it "minimum [0, 5) for [0..4]" $
      runSegTree [0 .. 4] [SegQuery 0 5] `shouldBe` U.fromList [0 :: Min Int]
    it "minimum [3, 4) for [0..4]" $
      runSegTree [0 .. 4] [SegQuery 3 4] `shouldBe` U.fromList [3 :: Min Int]
    it "minimum [3, 4) for update 3 (-1) [0..4]" $
      runSegTree [0 .. 4] [SegUpdate 3 (-1), SegQuery 3 4]
        `shouldBe` U.fromList [(-1) :: Min Int]
    it "minimum [0, 5) for update 3 (-1) [0..4]" $
      runSegTree [0 .. 4] [SegUpdate 3 (-1), SegQuery 0 5]
        `shouldBe` U.fromList [(-1) :: Min Int]
    it "minimum [0, 1) for [0]" $
      runSegTree [0] [SegQuery 0 1]
        `shouldBe` U.fromList [0 :: Min Int]
    it "minimum [0, 1) for update 0 (-1) [0]" $
      runSegTree [0] [SegUpdate 0 (-1), SegQuery 0 1]
        `shouldBe` U.fromList [(-1) :: Min Int]
  describe "extendToPowerOfTwo" $ do
    it "f 0 == 1" $ extendToPowerOfTwo 0 `shouldBe` 1
    it "f 1 == 1" $ extendToPowerOfTwo 1 `shouldBe` 1
    it "f 2 == 2" $ extendToPowerOfTwo 2 `shouldBe` 2
    it "f 3 == 4" $ extendToPowerOfTwo 3 `shouldBe` 4
    prop "popCount (f x) == 1" prop_powerOfTwo
    prop "x <= f x" prop_greaterThanOrEqual
    prop "popCount (f x / 2) /= 1 || f x / 2 < x" prop_least

data SegTreeQuery a
  = SegUpdate !Int !a
  | SegQuery !Int !Int
  deriving (Show)

instance Arbitrary a => Arbitrary (SegTreeQuery a) where
  arbitrary = do
    arbitrary >>= \case
      True -> SegUpdate <$> arbitrary <*> arbitrary
      False -> SegQuery <$> arbitrary <*> arbitrary

runSegTree ::
  (Monoid a, G.Vector v a) =>
  v a ->
  V.Vector (SegTreeQuery a) ->
  v a
runSegTree vec queries = G.create $ do
  seg <- buildSegTree vec
  res <- GM.replicate (V.length queries) mempty
  size <-
    V.foldM'
      ( \acc -> \case
          SegUpdate k v -> do
            writeSegTree seg k v
            return acc
          SegQuery l r -> do
            mappendFromTo seg l r >>= GM.unsafeWrite res acc
            return $ acc + 1
      )
      0
      queries
  return $ GM.take size res

runNaiveSegTree ::
  (Monoid a) =>
  V.Vector a ->
  V.Vector (SegTreeQuery a) ->
  V.Vector a
runNaiveSegTree vec queries = V.create $ do
  seg <- V.thaw vec
  res <- VM.replicate (V.length queries) mempty
  size <-
    V.foldM'
      ( \acc -> \case
          SegUpdate k v -> do
            VM.write seg k v
            return acc
          SegQuery l r -> do
            let go !i !m
                  | i < r = VM.read seg i >>= go (i + 1) . mappend m
                  | otherwise = VM.unsafeWrite res acc m
            go l mempty
            return $ acc + 1
      )
      0
      queries
  return $ VM.take size res

prop_runNaiveSegTree :: NonEmptyList [Int] -> [SegTreeQuery [Int]] -> Bool
prop_runNaiveSegTree (getNonEmpty -> xs) qs =
  runSegTree vec queries == runNaiveSegTree vec queries
  where
    vec = V.fromList xs
    len = V.length vec
    ix i = mod i len
    normalize (SegUpdate k v) = SegUpdate (ix k) v
    normalize (SegQuery l r) = SegQuery li (min (li + 1) ri)
      where
        li = ix l `min` ix l
        ri = ix l `min` ix r + 1
    queries =
      V.fromList $
        map normalize qs ++ [SegQuery 0 len]

prop_buildSegTree :: NonEmptyList [Int] -> Property
prop_buildSegTree (getNonEmpty -> xs) = monadicIO $ do
  (seg0', seg1') <- run $ do
    seg0 <- buildSegTree $ V.fromList xs
    seg0' <- V.freeze (getSegTree seg0)
    seg1 <- newSegTree (length xs)
    V.imapM_ (writeSegTree seg1) $ V.fromList xs
    seg1' <- V.freeze (getSegTree seg1)
    return (seg0', seg1')
  assert (seg0' == seg1')

prop_powerOfTwo :: NonNegative Int -> Bool
prop_powerOfTwo (getNonNegative -> x) =
  popCount (extendToPowerOfTwo x) == 1

prop_greaterThanOrEqual :: NonNegative Int -> Bool
prop_greaterThanOrEqual (getNonNegative -> x) = x <= extendToPowerOfTwo x

prop_least :: NonNegative Int -> Bool
prop_least (getNonNegative -> x) = not $ x <= prev && popCount prev == 1
  where
    res = extendToPowerOfTwo x
    prev = res `shiftR` 1
