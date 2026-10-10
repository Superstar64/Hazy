module Core.Tree.Class where

import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (Constraint)
import Core.Tree.Forall (ForallOver)
import Core.Tree.MethodInfo (MethodInfo (..))
import Core.Tree.Type (Type, TypeF, (-#>))
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    definition :: !(Definition (Local ':+ scope))
  }
  deriving (Show)

instance Shift0.Functor Class where
  map = Shift.mapDefault

instance Shift.Functor Class where
  map = Substitute.mapDefault

instance Substitute.Functor Class where
  map category Class {parameter, constraints, definition} =
    Class
      { parameter = Substitute.map category parameter,
        constraints = Substitute.map category <$> constraints,
        definition = Substitute.map (Substitute.Over category) definition
      }

type Definition = DefinitionF Void

newtype DefinitionF logical scope = Definition
  { methods :: Strict.Vector (ForallOver TypeF logical scope)
  }
  deriving (Show)

instance Shift0.Functor (DefinitionF logical) where
  map = Shift.mapDefault

instance Shift.Functor (DefinitionF logical) where
  map category Definition {methods} =
    Definition
      { methods = Shift.map category <$> methods
      }

instance (logical ~ Void) => Substitute.Functor (DefinitionF logical) where
  map category Definition {methods} =
    Definition
      { methods = Substitute.map category <$> methods
      }

kind :: Class scope -> Type scope
kind Class {parameter} = parameter -#> Type.Constraint

info :: Class scope -> MethodInfo scope
info Class {constraints} = MethodInfo {constraintCount = length constraints}
