{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Check.Temporary.Type where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Error (uncheckable, universeMustBeSmall, unsupportedFeatureStrictFunctions)
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Local as Local.Table
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Table.Type as Type.Table
import qualified Stage2.Index.Type2 as Type2
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Type as Solved
import qualified Stage2.Tree.Type as Stage2 (Synonym (..), Type (..))
import Stage2.Check.Context (Context (..))
import qualified Stage2.Check.Context as Context
import qualified Stage2.Check.DataInstance as DataInstance
import qualified Stage2.Check.LocalBinding as LocalBinding (LocalBinding (..))
import Stage2.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Stage2.Check.TypeBinding as TypeBinding
import qualified Stage2.Check.Simple.Data as Simple.Data
import Stage2.Check.Simple.Type (lift)
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} Stage4.Tree.TypeDeclaration as Simple (assumeData)

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

check :: Context s scope -> Unify.Type s scope -> Stage2.Type Position Resolve scope -> ST s (Type s scope)
check context@Context {localEnvironment, typeEnvironment} kind = \case
  Stage2.Variable {startPosition, variable} -> case localEnvironment Local.Table.! variable of
    LocalBinding.Rigid {rigid}
      | rigid <- lift rigid -> do
          Unify.unify context startPosition kind rigid
          pure $ Variable {startPosition, variable}
    LocalBinding.Wobbly {wobbly} -> do
      Unify.unify context startPosition kind wobbly
      pure Variable {startPosition, variable}
  Stage2.Constructor {startPosition, constructorPosition, constructor} -> do
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
  Stage2.Tuple {startPosition, elements} -> do
    elements <- traverse (check context Unify.typex) elements
    Unify.unify context startPosition kind Unify.typex
    pure Tuple {startPosition, elements}
  Stage2.Call {startPosition, function, argument} -> do
    level <- Unify.fresh Unify.universe
    parameterType <- Unify.fresh (Unify.typeWith level)
    function <- check context (Unify.function parameterType kind) function
    argument <- check context parameterType argument
    pure Call {startPosition, function, argument}
  Stage2.Function {startPosition, parameter, operatorPosition, result} -> do
    level <- Unify.fresh Unify.universe
    level' <- Unify.fresh Unify.universe
    parameter <- check context (Unify.typeWith level) parameter
    result <- check context (Unify.typeWith level') result
    Unify.unify context operatorPosition (Unify.typeWith level') kind
    pure Function {startPosition, operatorPosition, parameter, result}
  Stage2.List {startPosition, element} -> do
    element <- check context Unify.typex element
    Unify.unify context startPosition kind Unify.typex
    pure List {startPosition, element}
  Stage2.SmallType {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure SmallType {startPosition}
  Stage2.StrictFunction {operatorPosition} ->
    unsupportedFeatureStrictFunctions operatorPosition
  Stage2.LiftedList {startPosition, items} -> do
    inner <- Unify.fresh Unify.typex
    Unify.unify context startPosition kind (Unify.listWith inner)
    items <- traverse (check context inner) items
    pure LiftedList {startPosition, items}
  Stage2.Type {startPosition, universe} -> case universe of
    Stage2.Small {} -> do
      Unify.unify context startPosition kind Unify.kind
      pure SmallType {startPosition}
    _ -> universeMustBeSmall startPosition
  Stage2.Constraint {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure Constraint {startPosition}
  Stage2.Small {startPosition} -> uncheckable startPosition
  Stage2.Large {startPosition} -> uncheckable startPosition
  Stage2.Universe {startPosition} -> uncheckable startPosition
  Stage2.Levity {startPosition} -> do
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
            Strict.Nothing -> Stage2.NoSynonym
            Strict.Just synonym -> Stage2.Synonym synonym
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
