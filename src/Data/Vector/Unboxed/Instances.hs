{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Unboxed.Instances where

#if !MIN_VERSION_vector(0,12,1)
import Control.Monad (liftM)
import Data.Coerce (coerce)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Monoid (Dual(..),Sum(..),Product(..))
import Data.Semigroup (Min(..),Max(..),First(..),Last(..))
#endif

#if !MIN_VERSION_vector(0,12,1)
newtype instance UM.MVector s (Sum a) = MV_Sum (UM.MVector s a)
newtype instance U.Vector (Sum a) = V_Sum  (U.Vector a)
instance (U.Unbox a) => U.Unbox (Sum a)
instance (U.Unbox a) => GM.MVector UM.MVector (Sum a) where
    basicLength (MV_Sum v) = GM.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Sum v) = MV_Sum $ GM.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Sum v1) (MV_Sum v2) = GM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Sum `liftM` GM.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Sum v) = GM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MV_Sum `liftM` GM.basicUnsafeReplicate n (coerce x)
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MV_Sum v) i = coerce `liftM` GM.basicUnsafeRead v i
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Sum v) i x = GM.basicUnsafeWrite v i (coerce x)
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_Sum v) = GM.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MV_Sum v) x = GM.basicSet v (coerce x)
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MV_Sum v1) (MV_Sum v2) = GM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_Sum v1) (MV_Sum v2) = GM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_Sum v) n = MV_Sum `liftM` GM.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Sum a) where
    basicUnsafeFreeze (MV_Sum v) = V_Sum `liftM` G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Sum v) = MV_Sum `liftM` G.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Sum v) = G.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Sum v) = V_Sum $ G.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Sum v) i = coerce `liftM` G.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Sum mv) (V_Sum v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}

newtype instance UM.MVector s (Product a) = MV_Product (UM.MVector s a)
newtype instance U.Vector (Product a) = V_Product  (U.Vector a)
instance (U.Unbox a) => U.Unbox (Product a)
instance (U.Unbox a) => GM.MVector UM.MVector (Product a) where
    basicLength (MV_Product v) = GM.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Product v) = MV_Product $ GM.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Product v1) (MV_Product v2) = GM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Product `liftM` GM.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Product v) = GM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MV_Product `liftM` GM.basicUnsafeReplicate n (coerce x)
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MV_Product v) i = coerce `liftM` GM.basicUnsafeRead v i
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Product v) i x = GM.basicUnsafeWrite v i (coerce x)
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_Product v) = GM.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MV_Product v) x = GM.basicSet v (coerce x)
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MV_Product v1) (MV_Product v2) = GM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_Product v1) (MV_Product v2) = GM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_Product v) n = MV_Product `liftM` GM.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Product a) where
    basicUnsafeFreeze (MV_Product v) = V_Product `liftM` G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Product v) = MV_Product `liftM` G.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Product v) = G.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Product v) = V_Product $ G.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Product v) i = coerce `liftM` G.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Product mv) (V_Product v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}

newtype instance UM.MVector s (Min a) = MV_Min (UM.MVector s a)
newtype instance U.Vector (Min a) = V_Min  (U.Vector a)
instance (U.Unbox a) => U.Unbox (Min a)
instance (U.Unbox a) => GM.MVector UM.MVector (Min a) where
    basicLength (MV_Min v) = GM.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Min v) = MV_Min $ GM.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Min v1) (MV_Min v2) = GM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Min `liftM` GM.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Min v) = GM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MV_Min `liftM` GM.basicUnsafeReplicate n (coerce x)
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MV_Min v) i = coerce `liftM` GM.basicUnsafeRead v i
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Min v) i x = GM.basicUnsafeWrite v i (coerce x)
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_Min v) = GM.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MV_Min v) x = GM.basicSet v (coerce x)
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MV_Min v1) (MV_Min v2) = GM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_Min v1) (MV_Min v2) = GM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_Min v) n = MV_Min `liftM` GM.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Min a) where
    basicUnsafeFreeze (MV_Min v) = V_Min `liftM` G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Min v) = MV_Min `liftM` G.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Min v) = G.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Min v) = V_Min $ G.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Min v) i = coerce `liftM` G.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Min mv) (V_Min v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}

newtype instance UM.MVector s (Max a) = MV_Max (UM.MVector s a)
newtype instance U.Vector (Max a) = V_Max  (U.Vector a)
instance (U.Unbox a) => U.Unbox (Max a)
instance (U.Unbox a) => GM.MVector UM.MVector (Max a) where
    basicLength (MV_Max v) = GM.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (MV_Max v) = MV_Max $ GM.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicOverlaps (MV_Max v1) (MV_Max v2) = GM.basicOverlaps v1 v2
    {-# INLINE basicOverlaps #-}
    basicUnsafeNew n = MV_Max `liftM` GM.basicUnsafeNew n
    {-# INLINE basicUnsafeNew #-}
    basicInitialize (MV_Max v) = GM.basicInitialize v
    {-# INLINE basicInitialize #-}
    basicUnsafeReplicate n x = MV_Max `liftM` GM.basicUnsafeReplicate n (coerce x)
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeRead (MV_Max v) i = coerce `liftM` GM.basicUnsafeRead v i
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeWrite (MV_Max v) i x = GM.basicUnsafeWrite v i (coerce x)
    {-# INLINE basicUnsafeWrite #-}
    basicClear (MV_Max v) = GM.basicClear v
    {-# INLINE basicClear #-}
    basicSet (MV_Max v) x = GM.basicSet v (coerce x)
    {-# INLINE basicSet #-}
    basicUnsafeCopy (MV_Max v1) (MV_Max v2) = GM.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeMove (MV_Max v1) (MV_Max v2) = GM.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeGrow (MV_Max v) n = MV_Max `liftM` GM.basicUnsafeGrow v n
    {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Max a) where
    basicUnsafeFreeze (MV_Max v) = V_Max `liftM` G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeThaw (V_Max v) = MV_Max `liftM` G.basicUnsafeThaw v
    {-# INLINE basicUnsafeThaw #-}
    basicLength (V_Max v) = G.basicLength v
    {-# INLINE basicLength #-}
    basicUnsafeSlice i n (V_Max v) = V_Max $ G.basicUnsafeSlice i n v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeIndexM (V_Max v) i = coerce `liftM` G.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeCopy (MV_Max mv) (V_Max v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
    {-# INLINE elemseq #-}
#endif
