{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Check.Temporary.Type where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Builtin as Builtin
import {-# SOURCE #-} Core.Tree.TypeDeclaration as Simple (assumeData)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Error (uncheckable, universeMustBeSmall, unsupportedFeatureStrictFunctions)
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.LocalBinding as LocalBinding (LocalBinding (..))
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Simple.Type (lift)
import Semantic.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Local as Local.Table
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Index.Table.Type as Type.Table
import qualified Semantic.Index.Type2 as Type2
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Type as Semantic (Synonym (..), Type (..))
import qualified Semantic.Tree.Type as Solved
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Type s scope
  = Variable
      { startPosition :: !Position,
        variable :: !(Local.Index scope)
      }
  | Constructor
      { startPosition, constructorPosition :: !Position,
        constructor :: !(Type2.Index scope)
      }
  | Tuple
      { startPosition :: !Position,
        elements :: !(Strict.Vector2 (Type s scope))
      }
  | Call
      { startPosition :: !Position,
        function :: !(Type s scope),
        argument :: !(Type s scope)
      }
  | Function
      { startPosition, operatorPosition :: !Position,
        parameter :: !(Type s scope),
        result :: !(Type s scope)
      }
  | List
      { startPosition :: !Position,
        element :: !(Type s scope)
      }
  | LiftedList
      { startPosition :: !Position,
        items :: !(Strict.Vector1 (Type s scope))
      }
  | SmallType {startPosition :: !Position}
  | Constraint {startPosition :: !Position}
  | Levity {startPosition :: !Position}

check :: Context s scope -> Unify.Type s scope -> Semantic.Type Position Resolve scope -> ST s (Type s scope)
check context@Context {localEnvironment, typeEnvironment} kind = \case
  Semantic.Variable {startPosition, variable} -> case localEnvironment Local.Table.! variable of
    LocalBinding.Rigid {rigid}
      | rigid <- lift rigid -> do
          Unify.unify context startPosition kind rigid
          pure $ Variable {startPosition, variable}
    LocalBinding.Wobbly {wobbly} -> do
      Unify.unify context startPosition kind wobbly
      pure Variable {startPosition, variable}
  Semantic.Constructor {startPosition, constructorPosition, constructor} -> do
    kind' <- Builtin.kind (pure . lift) indexType indexLift constructor
    Unify.unify context constructorPosition kind kind'
    pure Constructor {startPosition, constructorPosition, constructor}
    where
      indexType constructor
        | TypeBinding {kind} <- typeEnvironment Type.Table.! constructor =
            do
              kind <- kind
              case kind of
                TypeBinding.Rigid kind -> pure $ lift kind
                TypeBinding.Wobbly kind -> pure kind
      indexLift constructor@Constructor.Index {typeIndex} = do
        datax <- do
          let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
          datax <- Builtin.index pure get typeIndex
          Simple.Data.instanciate context constructorPosition datax
        pure $ DataInstance.constructorFunction datax constructor
  Semantic.Tuple {startPosition, elements} -> do
    elements <- traverse (check context Unify.typex) elements
    Unify.unify context startPosition kind Unify.typex
    pure Tuple {startPosition, elements}
  Semantic.Call {startPosition, function, argument} -> do
    level <- Unify.fresh Unify.universe
    parameterType <- Unify.fresh (Unify.typeWith level)
    function <- check context (Unify.function parameterType kind) function
    argument <- check context parameterType argument
    pure Call {startPosition, function, argument}
  Semantic.Function {startPosition, parameter, operatorPosition, result} -> do
    level <- Unify.fresh Unify.universe
    level' <- Unify.fresh Unify.universe
    parameter <- check context (Unify.typeWith level) parameter
    result <- check context (Unify.typeWith level') result
    Unify.unify context operatorPosition (Unify.typeWith level') kind
    pure Function {startPosition, operatorPosition, parameter, result}
  Semantic.List {startPosition, element} -> do
    element <- check context Unify.typex element
    Unify.unify context startPosition kind Unify.typex
    pure List {startPosition, element}
  Semantic.SmallType {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure SmallType {startPosition}
  Semantic.StrictFunction {operatorPosition} ->
    unsupportedFeatureStrictFunctions operatorPosition
  Semantic.LiftedList {startPosition, items} -> do
    inner <- Unify.fresh Unify.typex
    Unify.unify context startPosition kind (Unify.listWith inner)
    items <- traverse (check context inner) items
    pure LiftedList {startPosition, items}
  Semantic.Type {startPosition, universe} -> case universe of
    Semantic.Small {} -> do
      Unify.unify context startPosition kind Unify.kind
      pure SmallType {startPosition}
    _ -> universeMustBeSmall startPosition
  Semantic.Constraint {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure Constraint {startPosition}
  Semantic.Small {startPosition} -> uncheckable startPosition
  Semantic.Large {startPosition} -> uncheckable startPosition
  Semantic.Universe {startPosition} -> uncheckable startPosition
  Semantic.Levity {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure Levity {startPosition}

solve :: Context s scope -> Type s scope -> Unify.Solve s (Solved.Type Position Check scope)
solve context = \case
  Variable {startPosition, variable} -> do
    pure
      Solved.Variable
        { startPosition,
          variable
        }
  Constructor {startPosition, constructorPosition, constructor} -> do
    synonym <- Unify.liftST $ Context.lookupSynonym context constructor
    pure
      Solved.Constructor
        { startPosition,
          constructorPosition,
          constructor,
          synonym = case synonym of
            Strict.Nothing -> Semantic.NoSynonym
            Strict.Just synonym -> Semantic.Synonym synonym
        }
  Tuple {startPosition, elements} -> do
    elements <- traverse (solve context) elements
    pure $ Solved.Tuple {startPosition, elements}
  Call {startPosition, function, argument} -> do
    function <- solve context function
    argument <- solve context argument
    pure $ Solved.Call {startPosition, function, argument}
  Function {startPosition, operatorPosition, parameter, result} -> do
    parameter <- solve context parameter
    result <- solve context result
    pure Solved.Function {startPosition, operatorPosition, parameter, result}
  List {startPosition, element} -> do
    element <- solve context element
    pure Solved.List {startPosition, element}
  LiftedList {startPosition, items} -> do
    items <- traverse (solve context) items
    pure $ Solved.LiftedList {startPosition, items}
  SmallType {startPosition} -> pure Solved.SmallType {startPosition}
  Constraint {startPosition} -> pure Solved.Constraint {startPosition}
  Levity {startPosition} -> pure Solved.Levity {startPosition}
