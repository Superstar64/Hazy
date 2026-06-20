module Semantic.Unify.SchemeOver where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import qualified Core.Tree.SchemeOver as Simple (SchemeOver (..))
import Data.Foldable (toList, traverse_)
import qualified Data.Kind as Kind
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Local as Table.Local
import qualified Semantic.Index.Table.Term as Table.Term
import qualified Semantic.Index.Table.Type as Table.Type
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..))
import Semantic.Unify.Class
  ( Collected (..),
    Collector (..),
    Functor (..),
    Generalizable (..),
    Instantiatable (..),
    Solve,
    Substitute (..),
    Zonk (..),
    Zonker (..),
    shiftDefault,
  )
import qualified Semantic.Unify.Class as Class
import Semantic.Unify.Constraint (Constraint (..))
import Semantic.Unify.Constraints (Constraints (..))
import qualified Semantic.Unify.Constraints as Constraints
import Semantic.Unify.Instanciation (Instanciation (..))
import Semantic.Unify.Type
  ( Box (..),
    Type (Logical, Variable),
    constrainWith,
    fresh,
    unshift,
  )
import qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)
import Prelude hiding (Functor, map)

data SchemeOver typex s scope = SchemeOver
  { parameters :: !(Strict.Vector (Type s scope)),
    constraints :: !(Constraints s scope),
    result :: !(typex s (Scope.Local ':+ scope))
  }

instance (Zonk typex) => Zonk (SchemeOver typex) where
  zonk zonker SchemeOver {parameters, constraints, result} = do
    parameters <- traverse (zonk zonker) parameters
    constraints <- zonk zonker constraints
    result <- zonk zonker result
    pure SchemeOver {parameters, constraints, result}

instance (Functor (typex s)) => Shift (SchemeOver typex s) where
  shift = shiftDefault

instance (Functor (typex s)) => Functor (SchemeOver typex s) where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = Class.map category <$> parameters,
        constraints = Class.map category constraints,
        result = Class.map (Class.Over category) result
      }

instanciateOver ::
  (Instantiatable typex) =>
  Context s scope ->
  Position ->
  SchemeOver typex s scope ->
  ST s (typex s scope, Instanciation s scope)
instanciateOver context position SchemeOver {parameters, constraints, result} = do
  fresh <- traverse fresh parameters
  instanciation <- case constraints of
    Constraints constraints -> do
      evidence <- for constraints $ \Constraint {classx, head, arguments} -> do
        head <- pure $ fresh Strict.Vector.! head
        arguments <- pure $ toList $ fmap (substitute $ Substitute fresh) arguments
        constrainWith context position classx head arguments
      pure $ Instanciation evidence
    None -> pure Mono
  pure $ (substitute (Substitute fresh) result, instanciation)

newtype Generalize typex s scopes = Generalize
  { runGeneralize ::
      forall scope.
      Context s (scope ':+ scopes) ->
      ST s (typex s (scope ':+ scopes))
  }

type Body ::
  (Kind.Type -> Environment -> Kind.Type) ->
  (Environment -> Kind.Type) ->
  Kind.Type ->
  Environment ->
  Kind.Type
data Body typex term s scope = (:::)
  { typex :: !(typex s scope),
    term :: !(Solve s (term scope))
  }

infix 5 :::

generalizeBody ::
  (Generalizable typex) =>
  Position ->
  Context s scope ->
  Generalize (Body typex term) s scope ->
  ST s (Body (SchemeOver typex) (Simple.SchemeOver term) s scope)
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
    SchemeOver
      { parameters,
        constraints = Constraints.None,
        result
      }
      ::: do
        parameters <- traverse (Type.solve position) parameters
        constraints <- Constraints.solve position Constraints.None
        result <- term
        pure
          Simple.SchemeOver
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
                _ <- unshift fail fail $ Logical reference
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

type SolveScheme :: (Kind.Type -> Environment -> Kind.Type) -> (Environment -> Kind.Type) -> Kind.Type
newtype SolveScheme source target
  = SolveScheme (forall s scope. Position -> source s scope -> Solve s (target scope))

solve ::
  SolveScheme source target ->
  Position ->
  SchemeOver source s scope ->
  Solve s (Simple.SchemeOver target scope)
solve (SolveScheme go) position SchemeOver {parameters, constraints, result} = do
  parameters <- traverse (Type.solve position) parameters
  constraints <- Constraints.solve position constraints
  result <- go position result
  pure $
    Simple.SchemeOver
      { parameters,
        constraints,
        result
      }

type MapScheme ::
  (Kind.Type -> Environment -> Kind.Type) ->
  (Kind.Type -> Environment -> Kind.Type) ->
  Kind.Type
newtype MapScheme typex typex' = MapScheme (forall s scope. typex s scope -> typex' s scope)

mapScheme :: MapScheme typex typex' -> SchemeOver typex s scope -> SchemeOver typex' s scope
mapScheme (MapScheme map) SchemeOver {parameters, constraints, result} =
  SchemeOver
    { parameters,
      constraints,
      result = map result
    }
