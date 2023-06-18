module Data.IntHeap.MaxTopKSpec (main, spec) where

import Data.IntHeap.MaxTopK
import qualified Data.List as L
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "insertMaxTopK" $ do
    it "insertMaxTopK 999 $ buildMaxTopK 4 [0..4]" $ do
      prop_naiveInsert 999 4 [0 .. 4]
    it "insertMaxTopK 1 $ buildMaxTopK 4 [0..4]" $ do
      prop_naiveInsert 1 4 [0 .. 4]
    it "insertMaxTopK 0 $ buildMaxTopK 4 [0..4]" $ do
      prop_naiveInsert 0 4 [0 .. 4]
    it "insertMaxTopK -999 $ buildMaxTopK 4 [0..4]" $ do
      prop_naiveInsert (-999) 4 [0 .. 4]
  describe "deleteMaxTopK" $ do
    describe "buildMaxTopK 3 [4,3,1,-1,-3,-4]" $ do
      let k = 3
          xs = [4, 3, 1, -1, -3, -4]
      prop "naive" $
        forAll (elements [-5 .. 5]) $ \x -> do
          prop_naiveDelete x k xs
    describe "buildMaxTopK 3 [2,2,1,0,0]" $ do
      let k = 3
          xs = [2, 2, 1, 0, 0]
      prop "naive" $
        forAll (elements [-5 .. 5]) $ \x -> do
          prop_naiveDelete x k xs
    describe "buildMaxTopK 3 [2,2,1,1,0]" $ do
      let k = 3
          xs = [2, 2, 1, 1, 0]
      prop "naive" $
        forAll (elements [-5 .. 5]) $ \x -> do
          prop_naiveDelete x k xs
    it "deleteMaxTopK 2 $ buildMaxTopK 3 [2,2,2,2,2]" $ do
      deleteMaxTopK 2 (buildMaxTopK 3 [2, 2, 2, 2, 2])
        `shouldBe` naiveDelete 2 3 [2, 2, 2, 2, 2]
  describe "exchangeMaxTopK" $ do
    describe "buildMaxTopK 3 [4,3,1,-1,-3,-4]" $ do
      let k = 3
          xs = [4, 3, 1, -1, -3, -4]
      prop "naive" $
        forAll
          ( (,)
              <$> elements [-5 .. 5]
              <*> elements [-5 .. 5]
          )
          $ \(old, new) ->
            prop_naiveExchange old new k xs

rsort :: [Int] -> [Int]
rsort = L.sortBy (flip compare)

prop_naiveInsert :: Int -> Int -> [Int] -> Expectation
prop_naiveInsert x k xs = do
  insertMaxTopK x (buildMaxTopK k xs)
    `shouldBe` naiveInsert x k xs

naiveInsert :: Int -> Int -> [Int] -> (Maybe Int, MaxTopK)
naiveInsert x k xs
  | old == new = (Nothing, buildMaxTopK k (x : xs))
  | otherwise = (Just (last old), buildMaxTopK k (x : xs))
  where
    old = take k $ rsort xs
    new = take k . rsort $ L.insert x xs

prop_naiveDelete :: Int -> Int -> [Int] -> Bool
prop_naiveDelete x k xs = do
  deleteMaxTopK x (buildMaxTopK k xs)
    == naiveDelete x k xs

naiveDelete :: Int -> Int -> [Int] -> (Maybe Int, MaxTopK)
naiveDelete x k xs
  | old == new = (Nothing, buildMaxTopK k $ L.delete x xs)
  | otherwise = (Just (last new), buildMaxTopK k $ L.delete x xs)
  where
    old = take k $ rsort xs
    new = take k . rsort $ L.delete x xs

prop_naiveExchange :: Int -> Int -> Int -> [Int] -> Bool
prop_naiveExchange old new k xs =
  exchangeMaxTopK old new (buildMaxTopK k xs)
    == naiveExchange old new k xs

naiveExchange :: Int -> Int -> Int -> [Int] -> (Maybe (Int, Int), MaxTopK)
naiveExchange old new k xs
  | old `notElem` xs = (Nothing, buildMaxTopK k xs)
  | oldList == newList = (Nothing, buildMaxTopK k xs')
  | otherwise =
    ( Just
        ( head $ newList L.\\ oldList
        , head $ oldList L.\\ newList
        )
    , buildMaxTopK k xs'
    )
  where
    xs' = L.insert new $ L.delete old xs

    oldList = take k $ rsort xs
    newList = take k $ rsort xs'
