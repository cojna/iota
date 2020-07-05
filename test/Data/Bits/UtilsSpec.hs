module Data.Bits.UtilsSpec (main, spec) where

import           Data.Bits
import           Data.Bits.Utils
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "bitReverse" $ do
        it "bitReverse 0x000f00f60000f6f6 = 0x6f6f00006f00f000" $ do
            bitReverse 0x000f00f60000f6f6 `shouldBe` 0x6f6f00006f00f000
        it "bitReverse 0 = 0" $ do
            bitReverse 0 `shouldBe` 0
        prop "bitReverse . bitReverse == id" prop_revrev

prop_revrev :: Int -> Bool
prop_revrev x = bitReverse (bitReverse x) == x
