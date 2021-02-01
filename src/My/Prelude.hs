{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module My.Prelude where

import Control.Monad.State.Strict
import Data.Bits
import Data.Bool
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Coerce
import qualified Data.Foldable as F
import Data.Functor.Identity
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import GHC.Exts (Int (..), uncheckedIShiftRL#)
import System.IO (hPutStrLn, stderr, stdout)

rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (stream 0 n)
{-# INLINE rep #-}

rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (stream 1 (n + 1))
{-# INLINE rep1 #-}

rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (streamR 0 n)
{-# INLINE rev #-}

rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1 #-}

stream :: (Monad m) => Int -> Int -> MS.Stream m Int
stream !l !r = MS.Stream step l
  where
    step x
        | x < r = return $ MS.Yield x (x + 1)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

streamR :: (Monad m) => Int -> Int -> MS.Stream m Int
streamR !l !r = MS.Stream step (r - 1)
  where
    step x
        | x >= l = return $ MS.Yield x (x - 1)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

stream' :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stream' !l !r !d = MS.Stream step l
  where
    step x
        | x < r = return $ MS.Yield x (x + d)
        | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream' #-}

infixl 8 `shiftRL`, `unsafeShiftRL`

shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}

unsafeShiftRL :: Int -> Int -> Int
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}

unlinesB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unlinesB f = G.foldr' ((<>) . (<> endlB) . f) mempty

unwordsB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unwordsB f vec
    | G.null vec = mempty
    | otherwise =
        f (G.head vec)
            <> G.foldr' ((<>) . (B.char7 ' ' <>) . f) mempty (G.tail vec)

gridB :: (G.Vector v a) => Int -> Int -> (a -> B.Builder) -> v a -> B.Builder
gridB h w f mat =
    F.foldMap
        ((<> endlB) . unwordsB f)
        [G.slice (i * w) w mat | i <- [0 .. h -1]]

sizedB :: (G.Vector v a) => (v a -> B.Builder) -> v a -> B.Builder
sizedB f vec = B.intDec (G.length vec) <> endlB <> f vec

yesnoB :: Bool -> B.Builder
yesnoB = bool (B.string7 "No") (B.string7 "Yes")

pairB :: (a -> B.Builder) -> (b -> B.Builder) -> (a, b) -> B.Builder
pairB f g (x, y) = f x <> B.char7 ' ' <> g y

showB :: (Show a) => a -> B.Builder
showB = B.string7 . show

showLnB :: (Show a) => a -> B.Builder
showLnB = B.string7 . flip shows "\n"

endlB :: B.Builder
endlB = B.char7 '\n'
{-# INLINE endlB #-}

runSolver :: Parser B.Builder -> IO ()
runSolver solver = do
    bs <- C.getContents
    case runStateT solver bs of
        Just (answer, rest) -> do
            unless (C.all isSpace rest) $ do
                C.hPutStrLn stderr rest
            B.hPutBuilder stdout answer
        Nothing -> hPutStrLn stderr "parse error"

type Parser a = StateT C.ByteString Maybe a

runParser :: Parser a -> C.ByteString -> Maybe (a, C.ByteString)
runParser = runStateT
{-# INLINE runParser #-}

int :: Parser Int
int = coerce $ C.readInt . C.dropWhile isSpace
{-# INLINE int #-}

int1 :: Parser Int
int1 = fmap (subtract 1) int
{-# INLINE int1 #-}

integer :: Parser Integer
integer = coerce $ C.readInteger. C.dropWhile isSpace
{-# INLINE integer #-}

integral :: (Integral a) => Parser a
integral = fmap fromIntegral int
{-# INLINE integral #-}

char :: Parser Char
char = coerce C.uncons
{-# INLINE char #-}

byte :: Parser Word8
byte = coerce B.uncons
{-# INLINE byte #-}

bytestring :: Parser C.ByteString
bytestring = do
    skipSpaces
    gets (C.findIndex isSpace) >>= \case
        Just i -> state (C.splitAt i)
        Nothing -> state (flip (,) C.empty)
{-# INLINE bytestring #-}

skipSpaces :: Parser ()
skipSpaces = modify' (C.dropWhile isSpace)
{-# INLINE skipSpaces #-}

vector :: (G.Vector v a) => Parser a -> Parser (v a)
vector p = G.unfoldr (runParser p) <$> takeLine
{-# INLINE vector #-}

vectorN :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorN n p = G.unfoldrN n (runParser p) <$> takeLine
{-# INLINE vectorN #-}

vectorH :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorH h p = G.unfoldrN h (runParser p) <$> takeLines h
{-# INLINE vectorH #-}

vectorW :: (G.Vector v a) => Int -> Parser a -> Parser (v a)
vectorW = vectorN
{-# INLINE vectorW #-}

vectorHW :: (G.Vector v a) => Int -> Int -> Parser a -> Parser (v a)
vectorHW h w p = G.unfoldrN (h * w) (runParser p) <$> takeLines h
{-# INLINE vectorHW #-}

gridHW :: Int -> Int -> Parser (U.Vector Char)
gridHW h w = U.unfoldrN (h * w) (runParser char) . C.filter (/= '\n') <$> takeLines h
{-# INLINE gridHW #-}

-- >>> runStateT takeLine (C.pack "abc")
-- Just ("abc","")
-- >>> runStateT takeLine (C.pack "abc\n")
-- Just ("abc","")
-- >>> runStateT takeLine (C.pack "abc\r\n")
-- Just ("abc\r","")
-- >>> runStateT takeLine C.empty
-- Just ("","")
-- >>> runStateT takeLine (C.pack "\n")
-- Just ("","")
-- >>> runStateT takeLine (C.pack "\n\n")
-- Just ("","\n")
takeLine :: Parser C.ByteString
takeLine = state $
    fmap (B.drop 1) . C.span (/= '\n')
{-# INLINE takeLine #-}

-- >>> runStateT (takeLines 1) (C.pack "abc\ndef\n")
-- Just ("abc\n","def\n")
-- >>> runStateT (takeLines 2) (C.pack "abc\ndef\n")
-- Just ("abc\ndef\n","")
-- >>> runStateT (takeLines 0) (C.pack "abc\ndef\n")
-- Just ("","abc\ndef\n")
-- >>> runStateT (takeLines 999) (C.pack "abc")
-- Just ("abc","")
takeLines :: Int -> Parser C.ByteString
takeLines n
    | n > 0 = do
        gets (drop (n - 1) . C.elemIndices '\n') >>= \case
            (i : _) -> state (C.splitAt (i + 1))
            [] -> state (flip (,) C.empty)
    | otherwise = pure C.empty
{-# INLINE takeLines #-}

neighbor4 :: (Applicative f) => Int -> Int -> Int -> (Int -> f ()) -> f ()
neighbor4 h w xy f =
    when (x /= 0) (f $ xy - w)
        *> when (y /= 0) (f $ xy - 1)
        *> when (y /= w - 1) (f $ xy + 1)
        *> when (x /= h - 1) (f $ xy + w)
  where
    (!x, !y) = quotRem xy w
{-# INLINE neighbor4 #-}

binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low high p = go low high
  where
    go !low !high
        | high <= low = return high
        | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid)
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE binarySearchM #-}

binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity $ binarySearchM low high (return . p)
{-# INLINE binarySearch #-}

radixSort :: U.Vector Int -> U.Vector Int
radixSort v = F.foldl' step v [0, 16, 32, 48]
  where
    mask k x = unsafeShiftRL x k .&. 0xffff
    step v k = U.create $ do
        pos <- UM.unsafeNew 0x10001
        UM.set pos 0
        U.forM_ v $ \x -> do
            UM.unsafeModify pos (+ 1) (mask k x + 1)
        rep 0xffff $ \i -> do
            fi <- UM.unsafeRead pos i
            UM.unsafeModify pos (+ fi) (i + 1)
        res <- UM.unsafeNew $ U.length v
        U.forM_ v $ \x -> do
            let !masked = mask k x
            i <- UM.unsafeRead pos masked
            UM.unsafeWrite pos masked $ i + 1
            UM.unsafeWrite res i x
        return res
{-# INLINE radixSort #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
    let !x = unsafeShiftRL xy 32
        !y = xy .&. 0xffffffff
     in (x, y)
{-# INLINE decode32x2 #-}
