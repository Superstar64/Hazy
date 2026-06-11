module Data.Maybe
  ( Maybe (..),
    maybe,
    isJust,
    isNothing,
    fromJust,
    fromMaybe,
    listToMaybe,
    maybeToList,
    catMaybes,
    mapMaybe,
  )
where

import Hazy.Prelude (placeholder)

isJust :: Maybe a -> Bool
isJust = placeholder

isNothing :: Maybe a -> Bool
isNothing = placeholder

fromJust :: Maybe a -> a
fromJust = placeholder

fromMaybe :: a -> Maybe a -> a
fromMaybe = placeholder

listToMaybe :: [a] -> Maybe a
listToMaybe = placeholder

maybeToList :: Maybe a -> [a]
maybeToList = placeholder

catMaybes :: [Maybe a] -> [a]
catMaybes = placeholder

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = placeholder
