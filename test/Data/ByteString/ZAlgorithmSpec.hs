{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.ZAlgorithmSpec (main, spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.ZAlgorithm
import qualified Data.Vector.Unboxed as U
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "z-algorithm" $ do
        prop "naive" prop_naive
        it "zAlgorithm \"ababab\" = [6,0,4,0,2,0]" $ do
            zAlgorithm "ababab" `shouldBe` U.fromList [6,0,4,0,2,0]
        it "zAlgorithm \"abc$xabcxx\" = [10,0,0,0,0,3,0,0,0,0]" $ do
            zAlgorithm "abc$xabcxx" `shouldBe` U.fromList [10,0,0,0,0,3,0,0,0,0]

prop_naive :: [Bool] -> Bool
prop_naive bs = zAlgorithm s == naiveZAlgorithm s
  where
    !s = C.pack $ map f bs
    f True = 'a'
    f False = 'b'

naiveZAlgorithm :: B.ByteString -> U.Vector Int
naiveZAlgorithm bs = U.generate n (\i -> naiveLCP bs $ B.drop i bs)
  where
    !n = B.length bs

naiveLCP :: B.ByteString -> B.ByteString -> Int
naiveLCP xs ys = go 0
  where
    !n = B.length xs `min` B.length ys
    go !i
        | i < n, B.unsafeIndex xs i == B.unsafeIndex ys i = go (i + 1)
        | otherwise = i
