module Control.Memo.Fix where

import           Data.Function
import qualified Data.Vector   as V

-- |
-- >>> :{
--  let fib memo 0 = 0
--      fib memo 1 = 1
--      fib memo i = memo (i - 1) + memo (i - 2)
--  in memoFix 100 fib 50
-- :}
-- 12586269025
memoFix :: Int -> ((Int -> a) -> Int -> a) -> Int -> a
memoFix n f = fix $ \memo -> (V.generate n (f memo) V.!)
