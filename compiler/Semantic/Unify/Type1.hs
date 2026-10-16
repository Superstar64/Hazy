-- | This module exists purely to work around `.hs-boot` module limitations.
-- Here, `constrain` returns evidence so it would make the `.hs-boot` modules
-- of `Semantic.Unify.Type` and `Semantic.Unify.Evidence` mutually recursive,
-- which GHC doesn't allow.
module Semantic.Unify.Type1 where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Core.Evidence.Functor (shiftLogical)
import Core.Substitute (logicalEvidence, logicalType, substituteType)
import qualified Core.Tree.Constraint as Simple (argument)
import qualified Core.Tree.Constraint as Simple.Constraint
import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import Core.Tree.Instanciation (InstanciationF (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation (InstanciationF (..))
import Core.Tree.Type (TypeF (..), (#))
import Data.Foldable (toList, traverse_)
import qualified Data.Map as Map
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Semantic.Check.Binding.Local as Local (Constraint (..), LocalBinding (..))
import Semantic.Check.Binding.Type (TypeBinding (TypeBinding))
import qualified Semantic.Check.Binding.Type as TypeBinding
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Table.Local as Local.Table
import qualified Semantic.Index.Table.Type as Type.Table
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Unify.Builtin as Builtin (constrain)
import {-# SOURCE #-} Semantic.Unify.Error (Error (..), abort)
import Semantic.Unify.Evidence (Evidence)
import qualified Semantic.Unify.Evidence as Evidence (Box (..), Logical (..))
import Semantic.Unify.Type
  ( Box (..),
    Delay (..),
    Logical (..),
    Type,
    fresh,
    typeCheck,
    unify,
    unshift,
  )
import Syntax.Position (Position)
import Prelude hiding (Functor, head, map)

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  ST s (Evidence s scope)
constrain context position classx term = constrainWith context position classx term []

constrainWith ::
  forall s scope.
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  [Type s scope] ->
  ST s (Evidence s scope)
constrainWith context_ position classx_ term_ arguments_ = constrainWith context_ classx_ term_ arguments_
  where
    constrainWith ::
      forall scope.
      Context s scope ->
      Type2.Index scope ->
      Type s scope ->
      [Type s scope] ->
      ST s (Evidence s scope)
    constrainWith context classx term@(Logical (Box reference)) arguments =
      readSTRef reference >>= \case
        Solved term -> constrainWith context classx term arguments
        Unsolved {kind, constraints, erasure} -> do
          case Map.lookup classx constraints of
            Nothing -> do
              universe <- fresh Universe
              target <- fresh (Type universe)
              real <- Context.lookupKind position context classx
              unify context position (Function target Constraint) real

              typeCheck context position target (foldl (#) term arguments)
              logical <- newSTRef Evidence.Unsolved {}
              let delay = Delay {arguments, evidence = Evidence.Logical (Evidence.Box logical)}
              writeSTRef reference $! Unsolved {kind, constraints = Map.insert classx delay constraints, erasure}
              pure $ Evidence.Logical (Evidence.Box logical)
            Just Delay {arguments = arguments', evidence}
              | length arguments == length arguments' -> do
                  zipWithM_ (unify context position) arguments arguments'
                  pure evidence
              | otherwise -> error "error argument length doesn't match"
    constrainWith context classx (Logical (Shift logical)) arguments = do
      arguments <- traverse (unshift context position) arguments
      let quit = abort position $ Unshift context (Constructor classx)
      classx <- Shift.partialUnshift quit classx
      shiftLogical <$> constrainWith (Shift.unshift context) classx (Logical logical) arguments
    constrainWith context@Context {localEnvironment} classx (Variable index) arguments
      | Local.Rigid {constraints} <- localEnvironment Local.Table.! index,
        Just Local.Constraint {arguments = arguments', evidence} <- Map.lookup classx constraints,
        arguments' <- [logicalType argument | argument <- toList arguments'],
        length arguments' == length arguments = do
          traverse_ (uncurry $ unify context position) (zip arguments arguments')
          pure (logicalEvidence evidence)
    constrainWith context@Context {typeEnvironment} (Type2.Index classx) (Constructor index) arguments
      | TypeBinding {classInstances} <- typeEnvironment Type.Table.! classx,
        Just instancex <- Map.lookup index classInstances = do
          dependencies <- instancex
          case dependencies of
            Constraints dependencies -> do
              arguments <- for dependencies $ \constraint@Simple.Constraint.Constraint {classx} ->
                let argument = substituteType (Vector.fromList arguments) (Simple.argument constraint)
                 in constrain context position classx argument
              pure $ Evidence.Variable variable (Instanciation arguments)
            None {levity} ->
              pure $ Evidence.Variable variable Instanciation.Mono {levity = logicalType levity}
      where
        variable = Evidence.Direct (Type2.Index classx) index
    constrainWith context@Context {typeEnvironment} classx (Constructor (Type2.Index index)) arguments
      | TypeBinding {dataInstances} <- typeEnvironment Type.Table.! index,
        Just instancex <- Map.lookup classx dataInstances = do
          dependencies <- instancex
          case dependencies of
            Constraints dependencies -> do
              arguments <- for dependencies $ \constraint@Simple.Constraint.Constraint {classx} ->
                let argument = substituteType (Vector.fromList arguments) (Simple.argument constraint)
                 in constrain context position classx argument
              pure $ Evidence.Variable variable (Instanciation arguments)
            None {levity} ->
              pure $ Evidence.Variable variable Instanciation.Mono {levity = logicalType levity}
      where
        variable = Evidence.Direct classx (Type2.Index index)
    constrainWith context classx (Call function argument) arguments =
      constrainWith context classx function (argument : arguments)
    constrainWith context classx (Function argument result) arguments = do
      typeCheck context position (Type Small) argument
      typeCheck context position (Type Small) result
      constrainWith context classx (Constructor Type2.Arrow `Call` argument `Call` result) arguments
    constrainWith context classx typex arguments = case typex of
      Constructor typex -> Builtin.constrain quit constrain classx typex arguments
      _ -> quit
      where
        constrain classx typex = constrainWith context classx typex []
        quit = abort position (Constrain context_ classx_ term_ arguments_)
