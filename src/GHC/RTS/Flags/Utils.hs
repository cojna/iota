module GHC.RTS.Flags.Utils where

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.RTS.Flags (RTSFlags)

foreign import ccall "&RtsFlags" rtsFlagsPtr :: Ptr RTSFlags

-- | defined in @rts\/Constants.h@
rtsBlockShift :: Int
rtsBlockShift = 12

-- | defined in @rts\/storage\/Block.h@
rtsBlockSize :: Word32
rtsBlockSize = 1 .<<. rtsBlockShift

-- | maxHeapSize (@-M@, default: @0@)
setRTSOptsM :: Word32 -> IO ()
setRTSOptsM m = pokeByteOff rtsFlagsPtr 28 $ div m rtsBlockSize

{- |
minAllocAreaSize (@-A@, default: @4 * 1024 * 1024@)

>>> setRTSOptsA $ 128 * 1024 * 1024
-}
setRTSOptsA :: Word32 -> IO ()
setRTSOptsA a = pokeByteOff rtsFlagsPtr 32 $ div a rtsBlockSize

-- | heapSizeSuggestion(@-H@, default: @0@)
setRTSOptsH :: Word32 -> IO ()
setRTSOptsH h = pokeByteOff rtsFlagsPtr 48 $ div h rtsBlockSize
