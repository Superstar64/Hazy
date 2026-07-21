module Semantic.Unify.Forall where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Core.Tree.Constraint (ConstraintF (..))
import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Constraints as Constraints (ConstraintsF (..))
import Core.Tree.Forall (ForallOver (..))
import qualified Core.Tree.Forall as Simple (ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF (Logical, Variable))
import qualified Core.Tree.Type as Type (TypeF (..))
import qualified Core.Tree.TypeLambda as Simple (TypeLambdaOver (..))
import Data.Foldable (toList, traverse_)
import qualified Data.Kind as Kind
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Local as Table.Local
import qualified Semantic.Index.Table.Term as Table.Term
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Unify.Constraints as Constraints (solve)
import qualified Semantic.Unify.Evidence as Evidence
import Semantic.Unify.Generalizable (Collected (..), Collector (..), Generalizable (..))
import Semantic.Unify.Solve (Solve (..))
import Semantic.Unify.Type
  ( Box (..),
    Logical (..),
    Type (..),
    constrainWith,
    fresh,
    unshift,
  )
import qualified Semantic.Unify.Type as Type (solve)
import Semantic.Unify.Zonk (Zonker (..), zonk)
import Syntax.Position (Position)
import Prelude hiding (Functor, map)

type Forall s = ForallOver TypeF (Logical s)

-- todo move this to Core
substitute ::
  Strict.Vector.Vector (TypeF (Logical s) scope) ->
  TypeF (Logical s) (Scope.Local ':+ scope) ->
  TypeF (Logical s) scope
substitute replacements (Variable index) = case index of
  Local.Local index -> replacements Strict.Vector.! index
  Local.Shift index -> Variable index
substitute replacements typex = case typex of
  Type.Logical (Shift logical) -> Type.Logical logical
  Type.Logical Box {} -> error "can't substitute box"
  Type.Constructor index -> Type.Constructor (Type2.unlocal index)
  Type.Call function argument -> Type.Call (substitute replacements function) (substitute replacements argument)
  Type.Function parameter result ->
    Type.Function (substitute replacements parameter) (substitute replacements result)
  Type.Type universe -> Type.Type (substitute replacements universe)
  Type.Constraint -> Type.Constraint
  Type.Small -> Type.Small
  Type.Large -> Type.Large
  Type.Universe -> Type.Universe
  Type.Levity -> Type.Levity

instanciate ::
  Context s scope ->
  Position ->
  Forall s scope ->
  ST s (Type s scope, InstanciationF (Evidence.Logical s) scope)
instanciate context position ForallOver {parameters, constraints, result} = do
  fresh <- traverse fresh parameters
  instanciation <- case constraints of
    Constraints constraints -> do
      evidence <- for constraints $ \Constraint {classx, head, arguments} -> do
        head <- pure $ fresh Strict.Vector.! head
        arguments <- pure $ toList $ fmap (substitute fresh) arguments
        constrainWith context position classx head arguments
      pure $ Instanciation evidence
    None -> pure Mono
  pure $ (substitute fresh result, instanciation)

newtype Generalize typex s scopes = Generalize
  { runGeneralize ::
      forall scope.
      Context s (scope ':+ scopes) ->
      ST s (typex s (scope ':+ scopes))
  }

type Body ::
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  (Environment -> Kind.Type) ->
  Kind.Type ->
  Environment ->
  Kind.Type
data Body typef term s scope = (:::)
  { typex :: !(typef (Logical s) scope),
    term :: !(Solve s (term scope))
  }

infix 5 :::

generalizeBody ::
  (Generalizable typex) =>
  Position ->
  Context s scope ->
  Generalize (Body typex term) s scope ->
  ST s (Body (ForallOver typex) (Simple.TypeLambdaOver term) s scope)
generalizeBody position context (Generalize run) = do
  typex ::: term <- run $ case context of
    Context {termEnvironment, localEnvironment, typeEnvironment} ->
      Context
        { termEnvironment = Table.Term.Local termEnvironment,
          localEnvironment = Table.Local.Local Vector.empty localEnvironment,
          typeEnvironment = Table.Type.Local typeEnvironment
        }
  candidates <- collect (Collector Mask.Runtime) typex
  traverse_ shiftUnwanted candidates
  boxes <- nub . catMaybes <$> traverse selectBox candidates
  parameters <- Strict.Vector.fromList <$> traverse parameter boxes
  zipWithM_ writeVariable [0 ..] boxes
  result <- zonk Zonker typex
  pure $
    ForallOver
      { parameters,
        constraints = Constraints.None,
        result
      }
      ::: do
        parameters <- traverse (Type.solve position) parameters
        constraints <- Constraints.solve position Constraints.None
        result <- term
        pure
          Simple.TypeLambdaOver
            { parameters,
              constraints,
              result
            }
  where
    shiftUnwanted :: Collected s (scope ':+ scopes) -> ST s ()
    shiftUnwanted = \case
      Collect reference ->
        readSTRef reference >>= \case
          Unsolved {kind, constraints}
            | null constraints -> do
                _ <- unshift fail fail kind
                pure ()
            | otherwise -> do
                _ <- unshift fail fail $ Logical (Box reference)
                pure ()
            where
              fail :: a
              fail = error "unsink can't fail"
          Solved {} -> pure ()
      Reach {} -> pure ()
    selectBox :: Collected s (scope ':+ scopes) -> ST s (Maybe (STRef s (Box s (scope ':+ scopes))))
    selectBox = \case
      Collect reference ->
        readSTRef reference >>= \case
          Unsolved {constraints}
            | null constraints -> pure $ Just reference
            | otherwise -> error "select box with unsolved"
          Solved {} -> pure Nothing
      Reach {} -> pure Nothing
    parameter :: STRef s (Box s (scope' ':+ scope)) -> ST s (Type s scope)
    parameter reference =
      readSTRef reference >>= \case
        Unsolved {kind} -> unshift fail fail kind
          where
            fail :: a
            fail = error "parameter can't fail"
        Solved {} -> error "bad kind"
    writeVariable :: Int -> STRef s (Box s (Scope.Local ':+ scopes)) -> ST s ()
    writeVariable variable reference =
      writeSTRef reference $ Solved $ Variable $ Local.Local variable

type SolveForall ::
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  Kind.Type
newtype SolveForall typef typef'
  = SolveForall (forall s scope. Position -> typef (Logical s) scope -> Solve s (typef' Vacuous scope))

solve ::
  SolveForall typef typef' ->
  Position ->
  ForallOver typef (Logical s) scope ->
  Solve s (Simple.ForallOver typef' Vacuous scope)
solve (SolveForall go) position ForallOver {parameters, constraints, result} = do
  parameters <- traverse (Type.solve position) parameters
  constraints <- Constraints.solve position constraints
  result <- go position result
  pure $
    Simple.ForallOver
      { parameters,
        constraints,
        result
      }

type MapForall ::
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  Kind.Type
newtype MapForall typef typef' = MapForall (forall s scope. typef (Logical s) scope -> typef' (Logical s) scope)

mapForall :: MapForall typef typef' -> ForallOver typef (Logical s) scope -> ForallOver typef' (Logical s) scope
mapForall (MapForall map) ForallOver {parameters, constraints, result} =
  ForallOver
    { parameters,
      constraints,
      result = map result
    }
