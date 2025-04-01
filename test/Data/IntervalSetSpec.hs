{-# LANGUAGE OverloadedLists #-}

module Data.IntervalSetSpec (main, spec) where

import Data.IntervalSet
import qualified Data.List as L
import qualified Test.Hspec.Core.Spec as H
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "insertIS" $ do
    it "insertIS 0 [] == [(0,0)]" $ do
      insertIS 0 [] `shouldBe` IntervalSet [(0, 0)]
    it "insertIS 1 [(0,0)] == [(0,1)]" $ do
      insertIS 1 [(0, 0)] `shouldBe` IntervalSet [(0, 1)]
    it "insertIS (-1) [(0,0)] == [(-1,0)]" $ do
      insertIS (-1) [(0, 0)] `shouldBe` IntervalSet [(-1, 0)]
    it "insertIS 2 [(0,0)] == [(0,0), (2,2)]" $ do
      insertIS 2 [(0, 0)] `shouldBe` IntervalSet [(0, 0), (2, 2)]
    it "insertIS (-2) [(0,0)] == [(-2,-2), (0,0)]" $ do
      insertIS (-2) [(0, 0)] `shouldBe` IntervalSet [(-2, -2), (0, 0)]
    it "insertIS 1 [(0,0), (4,4)] == [(0,1), (4,4)]" $ do
      insertIS 1 [(0, 0), (4, 4)] `shouldBe` IntervalSet [(0, 1), (4, 4)]
    it "insertIS 2 [(0,0), (4,4)] == [(0,0), (2,2), (4,4)]" $ do
      insertIS 2 [(0, 0), (4, 4)] `shouldBe` IntervalSet [(0, 0), (2, 2), (4, 4)]
    it "insertIS 3 [(0,0), (4,4)] == [(0,0), (3,4)]" $ do
      insertIS 3 [(0, 0), (4, 4)] `shouldBe` IntervalSet [(0, 0), (3, 4)]
    it "insertIS 2 [(0,1), (3,4)] == [(0,4)]" $ do
      insertIS 2 [(0, 1), (3, 4)] `shouldBe` IntervalSet [(0, 4)]
    it "insertIS 0 [(0,4)] == [(0,4)]" $ do
      insertIS 0 [(0, 4)] `shouldBe` IntervalSet [(0, 4)]
    it "insertIS 2 [(0,4)] == [(0,4)]" $ do
      insertIS 2 [(0, 4)] `shouldBe` IntervalSet [(0, 4)]
    it "insertIS 4 [(0,4)] == [(0,4)]" $ do
      insertIS 4 [(0, 4)] `shouldBe` IntervalSet [(0, 4)]
  describe "insertIntervalIS" $ do
    it "insertIntervalIS (0,1) [] == [(0,1)]" $ do
      insertIntervalIS (0, 1) [] `shouldBe` IntervalSet [(0, 1)]
    it "insertIntervalIS (-1,1) [(-2,-2), (0,0), (2,2)] == [(-2,2)]" $ do
      insertIntervalIS (-1, 1) [(-2, -2), (0, 0), (2, 2)]
        `shouldBe` IntervalSet [(-2, 2)]
    describe "[(0,0)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-2 .. 2], r <- [l .. 2]]
        (\(l, r) -> insertIntervalIS (l, r) [(0, 0)])
        (\(l, r) -> foldr @[] insertIS [(0, 0)] [l .. r])
    describe "[(-1,1)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-3 .. 3], r <- [l .. 3]]
        (\(l, r) -> insertIntervalIS (l, r) [(-1, 1)])
        (\(l, r) -> foldr @[] insertIS [(-1, 1)] [l .. r])
    describe "[(-2,-2), (2,2)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-4 .. 4], r <- [l .. 4]]
        (\(l, r) -> insertIntervalIS (l, r) [(-2, -2), (2, 2)])
        (\(l, r) -> foldr @[] insertIS emptyIS $ (-2) : 2 : [l .. r])
    describe "[(-4,-2), (2, 4)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-6 .. 6], r <- [l .. 6]]
        (\(l, r) -> insertIntervalIS (l, r) [(-4, -2), (2, 4)])
        ( \(l, r) ->
            foldr insertIS emptyIS $
              concat @[]
                [ [-4 .. (-2)]
                , [2 .. 4]
                , [l .. r]
                ]
        )
  describe "deleteIS" $ do
    it "deleteIS 0 [(0,0)] = []" $ do
      deleteIS 0 [(0, 0)] `shouldBe` []
    it "deleteIS (-1) [(0,0)] = [(0,0)]" $ do
      deleteIS (-1) [(0, 0)] `shouldBe` [(0, 0)]
    it "deleteIS 1 [(0,0)] = [(0,0)]" $ do
      deleteIS 1 [(0, 0)] `shouldBe` [(0, 0)]
    it "deleteIS 0 [] = []" $ do
      deleteIS 0 [] `shouldBe` []
    describe "[(0,0)]" $ do
      propNaiveByTable
        "naive test"
        [-2 .. 2]
        (\k -> deleteIS k [(0, 0)])
        ( \k ->
            foldr @[] insertIS emptyIS $
              filter (/= k) [0]
        )
    describe "[(-1,1)]" $ do
      propNaiveByTable
        "naive test"
        [-3 .. 3]
        (\k -> deleteIS k [(-1, 1)])
        ( \k ->
            foldr @[] insertIS emptyIS $
              filter (/= k) [-1 .. 1]
        )
    describe "[(-2,-2), (2,2))]" $ do
      propNaiveByTable
        "naive test"
        [-4 .. 4]
        (\k -> deleteIS k [(-2, -2), (2, 2)])
        ( \k ->
            foldr @[] insertIS emptyIS $
              filter (/= k) [-2, 2]
        )
    describe "[(-4,-2), (2,4))]" $ do
      propNaiveByTable
        "naive test"
        [-6 .. 6]
        (\k -> deleteIS k [(-4, -2), (2, 4)])
        ( \k ->
            foldr @[] insertIS emptyIS
              . filter (/= k)
              $ [-4 .. (-2)] <> [2 .. 4]
        )
  describe "deleteIntervalIS" $ do
    describe "[(0,0)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-2 .. 2], r <- [l .. 2]]
        (\(l, r) -> deleteIntervalIS (l, r) [(0, 0)])
        (\(l, r) -> foldr @[] deleteIS [(0, 0)] [l .. r])
    describe "[(-1,1)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-3 .. 3], r <- [l .. 3]]
        (\(l, r) -> deleteIntervalIS (l, r) [(-1, 1)])
        (\(l, r) -> foldr @[] deleteIS [(-1, 1)] [l .. r])
    describe "[(-2,-2), (2,2)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-4 .. 4], r <- [l .. 4]]
        (\(l, r) -> deleteIntervalIS (l, r) [(-2, -2), (2, 2)])
        (\(l, r) -> foldr @[] deleteIS [(-2, -2), (2, 2)] [l .. r])
    describe "[(-4,-2), (2, 4)]" $ do
      propNaiveByTable
        "naive test"
        [(l, r) | l <- [-6 .. 6], r <- [l .. 6]]
        (\(l, r) -> deleteIntervalIS (l, r) [(-4, -2), (2, 4)])
        ( \(l, r) ->
            foldr @[] deleteIS [(-4, -2), (2, 4)] [l .. r]
        )

propNaiveByTable ::
  (HasCallStack, Show a, Show b, Eq b) =>
  String ->
  [a] ->
  (a -> b) ->
  (a -> b) ->
  Spec
propNaiveByTable s table actual expect = it s $ do
  let results = [(x, actual x, expect x) | x <- table]
      counterexamples = L.filter (\(_, fx, gx) -> fx /= gx) results
  if length counterexamples == 0
    then
      H.Result
        { H.resultInfo = "+++ OK, passed " <> show (length results) <> " tests."
        , H.resultStatus = H.Success
        }
    else
      H.Result
        { H.resultInfo =
            unwords
              [ "passed"
              , show (length results - length counterexamples)
              , "tests, failed"
              , show (length counterexamples)
              , "tests."
              ]
        , H.resultStatus =
            H.Failure
              Nothing
              ( H.Reason $
                  "*** Failed! Falsified:\n"
                    <> foldMap
                      ( \(x, fx, gx) ->
                          "  " <> show x <> ": expected " <> show gx <> ", but got " <> show fx <> "\n"
                      )
                      counterexamples
              )
        }
