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
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import Semantic.Unify.Type (Box (..), Logical (..), unshift, unshiftBox)

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

type Zonk :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Constraint
class Zonk typef where
  zonk ::
    Zonker s s' ->
    Shift0.Category (scope1 ':+ scopex) scope ->
    typef (Logical s (scope1 ':+ scopex)) scope ->
    ST s (typef (Logical s' scopex) scope)

instance Zonk TypeF where
  zonk zonker@Zonker category = \case
    Type.Logical (Box reference) ->
      readSTRef reference >>= \case
        Solved solved -> Shift0.map category <$> zonk zonker Shift0.Id solved
        Unsolved {kind, constraints, erasure} -> do
          let fail = error "zonk can't fail"
          box <- unshiftBox (pure fail) (unshift fail fail) reference kind constraints erasure
          pure $ Type.Logical $ box
    Type.Logical (Shift logical) ->
      pure $ Type.Logical logical
    Type.Variable index -> pure $ Type.Variable index
    Type.Constructor index -> pure $ Type.Constructor index
    Type.Call function argument -> do
      function <- zonk zonker category function
      argument <- zonk zonker category argument
      pure $ Type.Call function argument
    Type.Function parameter result -> do
      parameter <- zonk zonker category parameter
      result <- zonk zonker category result
      pure $ Type.Function parameter result
    Type.Type universe -> Type.Type <$> zonk zonker category universe
    Type.Constraint -> pure Type.Constraint
    Type.Small -> pure Type.Small
    Type.Large -> pure Type.Large
    Type.Universe -> pure Type.Universe
    Type.Levity -> pure Type.Levity

instance Zonk ConstraintF where
  zonk zonker category Constraint.Constraint {classx, head, arguments} = do
    arguments <- traverse (zonk zonker (Shift0.Shift Shift0.:. category)) arguments
    pure Constraint.Constraint {classx, head, arguments}

instance Zonk ConstraintsF where
  zonk zonker category = \case
    Constraints.Constraints constraints -> do
      constraints <- traverse (zonk zonker category) constraints
      pure $ Constraints.Constraints constraints
    Constraints.None -> pure Constraints.None

instance (Zonk typef) => Zonk (ForallOver typef) where
  zonk zonker category Forall.ForallOver {parameters, constraints, result} = do
    parameters <- traverse (zonk zonker category) parameters
    constraints <- zonk zonker category constraints
    result <- zonk zonker (Shift0.Shift Shift0.:. category) result
    pure Forall.ForallOver {parameters, constraints, result}
