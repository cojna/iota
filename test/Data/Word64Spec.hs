{-# LANGUAGE ViewPatterns #-}

module Data.Word64Spec (main, spec) where

import           Data.Int
import           Data.Proxy
import           Data.Word
import           Data.Word64
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "decode64 . encode64 = id" $ do
        prop "Int" $ prop_decodeEncode (Proxy :: Proxy Int)
        prop "(Int, Int)" $ prop_decodeEncode (Proxy :: Proxy (Int, Int))
        prop "(Int, Int, Int)" $ prop_decodeEncode (Proxy :: Proxy (Int, Int, Int))
    describe "Word64Encode Int" $ do
        it "decode64 (encode64 (10^18)) == 10^18" $ do
            decode64 (encode64 $ pow 10 18) `shouldBe` pow 10 18
        it "decode64 (encode64 (-10^18)) == -10^18" $ do
            decode64 (encode64 $ - pow 10 18) `shouldBe` - pow 10 18
    describe "Word64Encode (Int, Int)" $ do
        it "decode64 (encode64 (-10^9, 10^9)) == (-10^9, 10^9)" $ do
            decode64 (encode64 (- pow 10 9, pow 10 9)) `shouldBe` (- pow 10 9, pow 10 9)
    describe "Word64Encode (Int, Int, Int)" $ do
        it "decode64 (encode64 (-10^6, 0, 10^6)) == (-10^6, 0, 10^6)" $ do
            decode64 (encode64 (- pow 10 6, (0 :: Int), pow 10 6)) `shouldBe` (- pow 10 6, (0 :: Int), pow 10 6)

pow :: Int -> Int -> Int
pow = (^)

prop_decodeEncode :: (Eq a, Word64Encode a) => Proxy a -> a -> Bool
prop_decodeEncode _ x = decode64 (encode64 x) == x
