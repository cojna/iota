{-# LANGUAGE CPP #-}

module Test.Prelude.Compat where

#if !MIN_VERSION_QuickCheck(2,13,0)
import Data.Coerce
import Test.QuickCheck

newtype Negative a = Negative {getNegative :: a}
    deriving (Eq, Show)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Negative a) where
    arbitrary = coerce (arbitrary @a `suchThat` (< 0))
#endif
