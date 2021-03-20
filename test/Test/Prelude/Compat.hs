{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Prelude.Compat where

import Data.Coerce
import Test.QuickCheck

#if !MIN_VERSION_QuickCheck(2,13,0)
newtype Negative a = Negative {getNegative :: a}
    deriving (Eq, Show)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Negative a) where
    arbitrary = coerce (arbitrary @a `suchThat` (< 0))
#endif
