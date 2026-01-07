module Stage2.Shift
  ( Shift (..),
    Category (..),
    Functor (..),
    mapmap,
    shiftDefault,
    PartialUnshift (..),
    Unshift (..),
  )
where

import qualified Data.Map as Map
import Data.Void (Void)
import Stage2.Scope (Environment ((:+)))
import Prelude hiding (Functor, id, map, (.))

class Shift f where
  shift :: f scopes -> f (scope ':+ scopes)

data Category scope scope' where
  Id :: Category scope scope
  Shift :: Category scopes (scope ':+ scopes)
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  (:.) :: Category scope' scope'' -> Category scope scope' -> Category scope scope''
  Rotate :: Category (scope1 ':+ scope2 ':+ scopes) (scope2 ':+ scope1 ':+ scopes)
  Unshift :: Void -> Category (scope ':+ scopes) scopes

infixr 9 :.

monotonic :: Category scope scope' -> Bool
monotonic = \case
  Id -> True
  Shift -> True
  Over category -> monotonic category
  category1 :. category2 -> monotonic category1 && monotonic category2
  Rotate -> False
  Unshift _ -> True

class (Shift f) => Functor f where
  map :: Category scope scope' -> f scope -> f scope'

mapmap :: (Functor f, Ord (f scope')) => Category scope scope' -> Map.Map (f scope) a -> Map.Map (f scope') a
mapmap category
  | monotonic category = Map.mapKeysMonotonic (map category)
  | otherwise = Map.mapKeys (map category)

shiftDefault :: (Functor f) => f scopes -> f (scope ':+ scopes)
shiftDefault = map Shift

class PartialUnshift f where
  partialUnshift :: (Applicative m) => m Void -> f (scope ':+ scopes) -> m (f scopes)

class Unshift f where
  unshift :: f (scope ':+ scopes) -> f scopes
