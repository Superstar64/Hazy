module Stage3.Unify.SchemeOver where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Data.Foldable (toList, traverse_)
import qualified Data.Kind as Kind
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.STRef (STRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Local as Table.Local
import qualified Stage2.Index.Table.Term as Table.Term
import qualified Stage2.Index.Table.Type as Table.Type
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..))
import Stage3.Check.Context (Context (..))
import Stage3.Unify.Class
  ( Collected (..),
    Collector (..),
    Functor (..),
    Generalizable (..),
    Instantiatable (..),
    Substitute (..),
    Zonk (..),
    Zonker (..),
    shiftDefault,
  )
import qualified Stage3.Unify.Class as Class
import Stage3.Unify.Constraint (Constraint (..))
import qualified Stage3.Unify.Constraint as Constraint
import Stage3.Unify.Instanciation (Instanciation (..))
import Stage3.Unify.Type
  ( Box (..),
    Type (Logical, Variable),
    constrainWith,
    fresh,
    unshift,
  )
import qualified Stage3.Unify.Type as Type
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import Prelude hiding (Functor, map)

data SchemeOver typex s scope = SchemeOver
  { parameters :: !(Strict.Vector (Type s scope)),
    constraints :: !(Strict.Vector (Constraint s scope)),
    result :: !(typex s (Scope.Local ':+ scope))
  }

instance (Zonk typex) => Zonk (SchemeOver typex) where
  zonk zonker SchemeOver {parameters, constraints, result} = do
    parameters <- traverse (zonk zonker) parameters
    constraints <- traverse (zonk zonker) constraints
    result <- zonk zonker result
    pure SchemeOver {parameters, constraints, result}

instance (Functor (typex s)) => Shift (SchemeOver typex s) where
  shift = shiftDefault

instance (Functor (typex s)) => Functor (SchemeOver typex s) where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = Class.map category <$> parameters,
        constraints = Class.map category <$> constraints,
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
  evidence <- for constraints $ \Constraint {classx, head, arguments} -> do
    head <- pure $ fresh Strict.Vector.! head
    arguments <- pure $ toList $ fmap (substitute $ Substitute fresh) arguments
    constrainWith context position classx head arguments
  pure $ (substitute (Substitute fresh) result, Instanciation evidence)

newtype Generalize typex s scopes = Generalize
  { runGeneralize ::
      forall scope.
      Context s (scope ':+ scopes) ->
      ST s (typex s (scope ':+ scopes))
  }

generalizeOver ::
  forall typex s scopes.
  (Generalizable typex) =>
  Context s scopes ->
  Generalize typex s scopes ->
  ST s (SchemeOver typex s scopes)
generalizeOver context (Generalize run) = do
  wobbly <- run $ case context of
    Context {termEnvironment, localEnvironment, typeEnvironment} ->
      Context
        { termEnvironment = Table.Term.Local termEnvironment,
          localEnvironment = Table.Local.Local Vector.empty localEnvironment,
          typeEnvironment = Table.Type.Local typeEnvironment
        }
  candidates <- collect Collector wobbly
  traverse_ shiftUnwanted candidates
  boxes <- nub . catMaybes <$> traverse selectBox candidates
  parameters <- Strict.Vector.fromList <$> traverse parameter boxes
  zipWithM_ writeVariable [0 ..] boxes
  result <- zonk Zonker wobbly
  pure
    SchemeOver
      { parameters,
        constraints = Strict.Vector.empty,
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

type Solve :: (Kind.Type -> Environment -> Kind.Type) -> (Environment -> Kind.Type) -> Kind.Type
newtype Solve source target = Solve
  { runSolve :: forall s scope. Position -> source s scope -> ST s (target scope)
  }

solve ::
  Solve source target ->
  Position ->
  SchemeOver source s scope ->
  ST s (Simple.SchemeOver target scope)
solve (Solve go) position SchemeOver {parameters, constraints, result} = do
  parameters <- traverse (Type.solve position) parameters
  constraints <- traverse (Constraint.solve position) constraints
  result <- go position result
  pure $
    Simple.SchemeOver
      { parameters,
        constraints,
        result
      }
