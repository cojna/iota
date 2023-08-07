{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.ManacherSpec (main, spec) where

import qualified Data.ByteString as B
import Data.ByteString.Manacher
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector.Unboxed as U
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "manacher" $ do
    prop "naive" prop_naive
    it "manacher \"a\" = [1]" $ do
      manacher "a" `shouldBe` U.fromList [1]
    it "manacher \"aba\" = [1,2,1]" $ do
      manacher "aba" `shouldBe` U.fromList [1, 2, 1]
    it "manacher \"a$b$b$a\" = [1,1,2,4,2,1,1]" $ do
      manacher "a$b$b$a" `shouldBe` U.fromList [1, 1, 2, 4, 2, 1, 1]

prop_naive :: ByteStringOf "ab" -> Property
prop_naive (getByteStringOf -> s) =
  odd (B.length s) ==> manacher s == naiveManacher s

naiveManacher :: B.ByteString -> U.Vector Int
naiveManacher bs = U.generate n $ naiveRadius bs
  where
    !n = B.length bs

naiveRadius :: B.ByteString -> Int -> Int
naiveRadius bs center = go 0
  where
    !n = B.length bs
    go !r
      | center - r >= 0
      , center + r < n
      , B.unsafeIndex bs (center - r) == B.unsafeIndex bs (center + r) =
          go (r + 1)
      | otherwise = r
