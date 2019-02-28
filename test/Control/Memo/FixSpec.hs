module Control.Memo.FixSpec where

import Test.Hspec
import Control.Memo.Fix

spec :: Spec
spec = do
    describe "memoFix" $ do
        it "calculate fib 50 with memoized recursion" $ do
            let fib memo 0 = 0
                fib memo 1 = 1
                fib memo i = memo (i - 1) + memo (i - 2)
            memoFix 100 fib 50 `shouldBe` 12586269025
