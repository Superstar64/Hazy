module Stage2.Shift
  ( Shift (..),
    Category (..),
    Functor (..),
    mapInstances,
    shiftDefault,
    PartialUnshift (..),
    Unshift (..),
  )
where

import qualified Data.Map as Map
import Data.Void (Void)
import {-# SOURCE #-} Stage2.Index.Type2 as Type2 (Index)
import Stage2.Scope (Environment ((:+)))
import Prelude hiding (Functor, id, map, (.))

class Shift f where
  shift :: f scopes -> f (scope ':+ scopes)

data Category scope scope' where
  Id :: Category scope scope
  Shift :: Category scopes (scope ':+ scopes)
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  (:.) :: Category scope' scope'' -> Category scope scope' -> Category scope scope''
  Unshift :: Void -> Category (scope ':+ scopes) scopes

infixr 9 :.

class (Shift f) => Functor f where
  map :: Category scope scope' -> f scope -> f scope'

mapInstances ::
  Category scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

shiftDefault :: (Functor f) => f scopes -> f (scope ':+ scopes)
shiftDefault = map Shift

class PartialUnshift f where
  partialUnshift :: (Applicative m) => m Void -> f (scope ':+ scopes) -> m (f scopes)

class Unshift f where
  unshift :: f (scope ':+ scopes) -> f scopes
