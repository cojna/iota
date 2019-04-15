{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
module Data.SegTree.VectorSpec (main, spec) where

import           Data.SegTree.Vector
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup as Semigroup
#endif
import qualified Data.Vector  as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic

main :: IO ()
main = hspec spec

#if MIN_VERSION_base(4,9,0)
instance Semigroup.Semigroup Int where
  (<>) = min
#endif

instance Monoid Int where
    mempty = maxBound
    {-# INLINE mempty #-}
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
    mappend = (Semigtoup.<>)
    {-# INLINE mappend #-}
#else
    mappend = min
    {-# INLINE mappend #-}
#endif

spec :: Spec
spec =
    describe "runSegTree" $ do
        it "works" $
            let res = runSegTree [1..5]
                    [ SegQuery 0 1
                    , SegQuery 4 5
                    , SegQuery 0 5
                    , SegQuery 1 4
                    , SegQuery 3 4
                    , SegUpdate 2 0
                    , SegQuery 0 1
                    , SegQuery 4 5
                    , SegQuery 0 5
                    , SegQuery 1 4
                    , SegQuery 3 4
                    , SegUpdate 0 0
                    , SegUpdate 4 0
                    , SegQuery 0 1
                    , SegQuery 3 4
                    , SegQuery 4 5
                    ]
            in res `shouldBe` U.fromList [1 :: Int,5,1,2,4,1,5,0,0,4,0,4,0]
