module Semantic.Shift0 where

import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Type as Type
import Semantic.Scope (Environment (..), Vacuous)
import Prelude hiding (Functor (..), map)

data Category scope scope' where
  Id :: Category scope scope
  Shift :: Category scopes (scope ':+ scopes)

class Functor f where
  map :: Category scope scope' -> f scope -> f scope'

instance Functor Term.Index where
  map Id index = index
  map Shift index = Term.Shift index

instance Functor Type.Index where
  map Id index = index
  map Shift index = Type.Shift index

instance Functor Evidence0.Index where
  map Id index = index
  map Shift index = Evidence0.Shift index

instance Functor Local.Index where
  map Id index = index
  map Shift index = Local.Shift index

instance Functor Vacuous where
  map _ = \case {}

shift :: (Functor f) => f scope -> f (scope' ':+ scope)
shift = map Shift
