module Semantic.Unify.Zonk where

import Control.Monad.ST (ST)
import Core.Tree.Constraint (ConstraintF)
import qualified Core.Tree.Constraint as Constraint
import Core.Tree.Constraints (ConstraintsF)
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.Forall (ForallOver)
import qualified Core.Tree.Forall as Forall
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import qualified Data.Kind as Kind
import Data.STRef (readSTRef)
import Semantic.Scope (Environment)
import Semantic.Shift0 (shift)
import Semantic.Unify.Type (Box (..), Logical (..))

-- |
-- This type used to help make sure zonks are type safe.
--
-- Conceptually, zonks could, in theory, be fully solving an AST while still
-- leaving it in the unsolved AST format. In which case, the state token would
-- be transformed into a fictional `Void` token
--
-- This isn't done at the moment however, hence the single Refl constructor.
--
-- Additionally, this also stops outside use of zonk.
data Zonker s s' where
  Zonker :: Zonker s s

type Zonk :: ((Environment -> Kind.Type) -> Environment -> Kind.Type) -> Kind.Constraint
class Zonk typef where
  zonk :: Zonker s s' -> typef (Logical s) scope -> ST s (typef (Logical s') scope)

instance Zonk TypeF where
  zonk Zonker = zonk
    where
      zonk :: TypeF (Logical s) scope -> ST s (TypeF (Logical s) scope)
      zonk = \case
        Type.Logical (Box reference) ->
          readSTRef reference >>= \case
            Solved solved -> zonk solved
            Unsolved {} -> pure $ Type.Logical $ Box reference
        Type.Logical (Shift logical) -> shift <$> zonk (Type.Logical logical)
        Type.Variable index -> pure $ Type.Variable index
        Type.Constructor index -> pure $ Type.Constructor index
        Type.Call function argument -> do
          function <- zonk function
          argument <- zonk argument
          pure $ Type.Call function argument
        Type.Function parameter result -> do
          parameter <- zonk parameter
          result <- zonk result
          pure $ Type.Function parameter result
        Type.Type universe -> Type.Type <$> zonk universe
        Type.Constraint -> pure Type.Constraint
        Type.Small -> pure Type.Small
        Type.Large -> pure Type.Large
        Type.Universe -> pure Type.Universe
        Type.Levity -> pure Type.Levity

instance Zonk ConstraintF where
  zonk zonker Constraint.Constraint {classx, head, arguments} = do
    arguments <- traverse (zonk zonker) arguments
    pure Constraint.Constraint {classx, head, arguments}

instance Zonk ConstraintsF where
  zonk zonker = \case
    Constraints.Constraints constraints -> do
      constraints <- traverse (zonk zonker) constraints
      pure $ Constraints.Constraints constraints
    Constraints.None -> pure Constraints.None

instance (Zonk typef) => Zonk (ForallOver typef) where
  zonk zonker Forall.ForallOver {parameters, constraints, result} = do
    parameters <- traverse (zonk zonker) parameters
    constraints <- zonk zonker constraints
    result <- zonk zonker result
    pure Forall.ForallOver {parameters, constraints, result}
