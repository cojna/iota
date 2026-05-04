{-# LANGUAGE TypeFamilies #-}

module Data.Queue.SWAG where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import GHC.Exts

{- |
>>> slidingWindowAggregationK 3 $ map (:"")['a'..'e']
["abc","bcd","cde"]
>>> slidingWindowAggregationK 100 $ map (:"")['a'..'e']
[]
-}
slidingWindowAggregationK :: (Semigroup a) => Int -> [a] -> [a]
slidingWindowAggregationK k _ | k <= 0 = []
slidingWindowAggregationK k xs
  | length ys0 == k = agg0 : snd (L.mapAccumL step queue0 ys)
  | otherwise = []
  where
    (ys0, ys) = splitAt k xs
    queue0 = fromList ys0
    !agg0 = sconcatQ queue0
    step !queue x = case unconsQ (snocQ queue x) of
      Just (_, queue') -> (queue', sconcatQ queue')
      Nothing -> undefined

data Agg a = Agg {getAggAcc :: !a, getAggItem :: !a}
  deriving (Eq, Show)

{- |
>>> Agg "abc" "a" <> Agg "def" "d"
Agg {getAggAcc = "abcdef", getAggItem = "a"}
-}
instance (Semigroup a) => Semigroup (Agg a) where
  (Agg accX x) <> (Agg accY _) = Agg (accX <> accY) x

data Queue a = Q [Agg a] [Agg a]
  deriving (Eq, Show)

emptyQ :: Queue a
emptyQ = Q [] []
{-# INLINE emptyQ #-}

nullQ :: Queue a -> Bool
nullQ (Q fs rs) = null fs && null rs
{-# INLINE nullQ #-}

singletonQ :: a -> Queue a
singletonQ x = Q [Agg x x] []
{-# INLINE singletonQ #-}

{- |
>>> unconsQ $ fromListQ ["a", "b", "c"]
Just ("a",Q [Agg {getAggAcc = "bc", getAggItem = "b"},Agg {getAggAcc = "c", getAggItem = "c"}] [])
>>> unconsQ $ emptyQ |> "a" |> "b" |> "c"
Just ("a",Q [Agg {getAggAcc = "bc", getAggItem = "b"},Agg {getAggAcc = "c", getAggItem = "c"}] [])
>>> unconsQ (emptyQ @String)
Nothing
-}
unconsQ :: (Semigroup a) => Queue a -> Maybe (a, Queue a)
unconsQ (Q (Agg _ f : fs) rs) = Just (f, Q fs rs)
unconsQ (Q [] []) = Nothing
unconsQ (Q [] rs) = case NE.nonEmpty rs of
  Nothing -> Nothing
  Just rs' -> case NE.reverse $ NE.scanl1 (\ !acc x -> x <> acc) $ fmap (\(Agg _ x) -> Agg x x) rs' of
    Agg _ f NE.:| fs -> Just (f, Q fs [])
{-# INLINE unconsQ #-}

snocQ :: (Semigroup a) => Queue a -> a -> Queue a
snocQ (Q fs rs) x = case rs of
  [] -> Q fs [Agg x x]
  (Agg acc _ : _) -> Q fs (Agg (acc <> x) x : rs)
{-# INLINE snocQ #-}

infixr 5 <|
infixl 5 |>

(|>) :: (Semigroup a) => Queue a -> a -> Queue a
(|>) = snocQ
{-# INLINE (|>) #-}

consQ :: (Semigroup a) => a -> Queue a -> Queue a
consQ x (Q fs rs) = case fs of
  [] -> Q [Agg x x] rs
  (Agg acc _ : _) -> Q (Agg (x <> acc) x : fs) rs
{-# INLINE consQ #-}

(<|) :: (Semigroup a) => a -> Queue a -> Queue a
(<|) = consQ
{-# INLINE (<|) #-}

{- |
>>> mconcatQ (("a" <| "b" <| emptyQ) |> "c" |> "d")
"abcd"
>>> mconcatQ (emptyQ @String)
""
-}
mconcatQ :: (Monoid a) => Queue a -> a
mconcatQ (Q fs rs) = f <> r
  where
    !f = foldr (const . getAggAcc) mempty fs
    !r = foldr (const . getAggAcc) mempty rs

{- |
>>> sconcatQ (("a" <| "b" <| emptyQ) |> "c" |> "d")
"abcd"
>>> sconcatQ (emptyQ @String)
*** Exception: Prelude.undefined
-}
sconcatQ :: (Semigroup a) => Queue a -> a
sconcatQ (Q (Agg acc _ : _) []) = acc
sconcatQ (Q [] (Agg acc _ : _)) = acc
sconcatQ (Q (Agg accF _ : _) (Agg accR _ : _)) = accF <> accR
sconcatQ (Q [] []) = undefined

{- |
>>> fromListQ ["a", "b", "c"]
Q [Agg {getAggAcc = "abc", getAggItem = "a"},Agg {getAggAcc = "bc", getAggItem = "b"},Agg {getAggAcc = "c", getAggItem = "c"}] []
-}
fromListQ :: (Semigroup a) => [a] -> Queue a
fromListQ xs = case NE.nonEmpty xs of
  Nothing -> Q [] []
  Just xs' -> Q (NE.toList . NE.scanr1 (\x !acc -> x <> acc) $ fmap (\x -> Agg x x) xs') []
{-# INLINE fromListQ #-}

instance (Semigroup a) => IsList (Queue a) where
  type Item (Queue a) = a
  fromList = fromListQ
  toList (Q fs rs) = map getAggItem fs <> reverse (map getAggItem rs)
