module Semantic.Shift0 where

import Data.Kind (Constraint, Type)
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Type as Type
import Semantic.Index.Type2 (Split (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Layout)
import Semantic.Scope (Environment (..), Vacuous)
import Semantic.Stage (Stage)
import Prelude hiding (Functor (..), map)

data Category scope scope' where
  Id :: Category scope scope
  Shift :: Category scopes (scope ':+ scopes)
  (:.) :: Category scope' scope'' -> Category scope scope' -> Category scope scope''

infixr 9 :.

class Functor f where
  map :: Category scope scope' -> f scope -> f scope'

instance Functor Term.Index where
  map Id index = index
  map Shift index = Term.Shift index
  map (after :. before) index = map after (map before index)

instance Functor Type.Index where
  map Id index = index
  map Shift index = Type.Shift index
  map (after :. before) index = map after (map before index)

instance Functor Type2.Index where
  map category index = case Type2.split index of
    Normal index -> Type2.Index $ map category index
    Constructor index -> Type2.Lifted $ map category index
    Builtin index -> index

instance Functor Evidence0.Index where
  map Id index = index
  map Shift index = Evidence0.Shift index
  map (after :. before) index = map after (map before index)

instance Functor Local.Index where
  map Id index = index
  map Shift index = Local.Shift index
  map (after :. before) index = map after (map before index)

instance Functor Vacuous where
  map _ = \case {}

shift :: (Functor f) => f scope -> f (scope' ':+ scope)
shift = map Shift

type TermFunctor :: (Layout -> Stage -> Environment -> Type) -> Constraint
class TermFunctor term where
  mapTerm :: Category scope scope' -> term layout stage scope -> term layout stage scope'
