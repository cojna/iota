module Data.Proxy.Util where

import           Data.Proxy

lift1 :: Proxy a -> (a -> b) -> (a -> b)
lift1 _ = id

lift2 :: Proxy a -> (a -> a -> b) -> (a -> a -> b)
lift2 _ = id

lift3 :: Proxy a -> (a -> a -> a -> b) -> (a -> a -> a -> b)
lift3 _ = id
