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

import Data.Bool (Bool)
import Prelude (error)

data Maybe a
  = Nothing
  | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = error "todo"

isJust :: Maybe a -> Bool
isJust = error "todo"

isNothing :: Maybe a -> Bool
isNothing = error "todo"

fromJust :: Maybe a -> a
fromJust = error "todo"

fromMaybe :: a -> Maybe a -> a
fromMaybe = error "todo"

listToMaybe :: [a] -> Maybe a
listToMaybe = error "todo"

maybeToList :: Maybe a -> [a]
maybeToList = error "todo"

catMaybes :: [Maybe a] -> [a]
catMaybes = error "todo"

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = error "todo"
