module Core.Tree.TypeLambda where

import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (Constraints, ConstraintsF (..))
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import Core.Tree.Type (Type)
import qualified Data.Kind as Kind
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Scope as Scope
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type TypeLambda = TypeLambdaOver Expression

data TypeLambdaOver term scope = TypeLambdaOver
  { parameters :: !(Strict.Vector (Type scope)),
    constraints :: !(Constraints scope),
    result :: !(term (Local ':+ scope))
  }

instance (Shift.Functor term) => Shift0.Functor (TypeLambdaOver term) where
  map = Shift.mapDefault

instance (Shift.Functor term) => Shift.Functor (TypeLambdaOver term) where
  map category TypeLambdaOver {parameters, constraints, result} =
    TypeLambdaOver
      { parameters = Shift.map category <$> parameters,
        constraints = Shift.map category constraints,
        result = Shift.map (Shift.Over category) result
      }

instance (Substitute.Functor term) => Substitute.Functor (TypeLambdaOver term) where
  map category TypeLambdaOver {parameters, constraints, result} =
    TypeLambdaOver
      { parameters = Substitute.map category <$> parameters,
        constraints = Substitute.map category constraints,
        result = Substitute.map (Substitute.Over category) result
      }

instance (Scope.Show term) => Scope.Show (TypeLambdaOver term) where
  showsPrec = showsPrec

instance (Scope.Show term) => Show (TypeLambdaOver term scope) where
  showsPrec _ TypeLambdaOver {parameters, constraints, result} =
    foldr
      (.)
      id
      [ showString "TypeLambdaOver { parameters = ",
        shows parameters,
        showString ", constraints = ",
        shows constraints,
        showString ", result = ",
        Scope.shows result,
        showString " }"
      ]

mono :: (Shift0.Functor term) => term scope -> TypeLambdaOver term scope
mono result =
  TypeLambdaOver
    { parameters = Strict.Vector.empty,
      constraints = None,
      result = shift result
    }

type Map :: (Environment -> Kind.Type) -> (Environment -> Kind.Type) -> Kind.Type
newtype Map typex typex' = Map (forall scope. typex scope -> typex' scope)

map :: Map typex typex' -> TypeLambdaOver typex scope -> TypeLambdaOver typex' scope
map (Map map) TypeLambdaOver {parameters, constraints, result} =
  TypeLambdaOver
    { parameters,
      constraints,
      result = map result
    }
