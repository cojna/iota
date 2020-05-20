module Geometry.Dim2.Instances where

import           Geometry.Dim2.Base
import           Geometry.Dim2.Circle
import           Test.Prelude

instance Arbitrary Point where
    arbitrary = P <$> arbitrary <*> arbitrary

instance Arbitrary Circle where
    arbitrary = Circle <$> arbitrary <*> arbitrary
