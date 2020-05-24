module Control.Memo.FixSpec (main, spec) where

import Control.Memo.Fix
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "memoFix" $ do
        it "calculate fib 50 with memoized recursion" $ do
            let fib memo 0 = 0
                fib memo 1 = 1
                fib memo i = memo (i - 1) + memo (i - 2)
            memoFix 100 fib 50 `shouldBe` 12586269025
    describe "memoFixMap" $ do
        it "calculate fib 50 with memoized recursion" $ do
            memoFixMap fibM 50 `shouldBe` 12586269025
    describe "memoFixIntMap" $ do
        it "calculate fib 50 with memoized recursion" $ do
            memoFixIntMap fibM 50 `shouldBe` 12586269025

fibM :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
fibM _ 0 = pure 0
fibM _ 1 = pure 1
fibM memo i = (+) <$> memo (i - 1) <*> memo (i - 2)
