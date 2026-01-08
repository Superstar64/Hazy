{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Temporary.Type where

import Control.Monad.ST (ST)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import qualified Data.Vector.Strict as Strict.Vector
import Error (uncheckable, universeMustBeSmall, unsupportedFeatureStrictFunctions)
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Local as Local.Table
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Table.Type as Type.Table
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Tree.Type as Stage2 (Type (..))
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import qualified Stage3.Check.LocalBinding as LocalBinding (LocalBinding (..))
import Stage3.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Stage3.Check.TypeBinding as TypeBinding
import {-# SOURCE #-} qualified Stage3.Simple.Builtin as Builtin
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Simple.Type (lift)
import {-# SOURCE #-} Stage3.Simple.TypeDeclaration as Simple (assumeData)
import qualified Stage3.Synonym as Synonym
import qualified Stage3.Tree.Type as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data Type s scope
  = Variable {variable :: !(Local.Index scope)}
  | Constructor {constructor :: !(Type2.Index scope)}
  | Tuple
      { elements :: !(Strict.Vector2 (Type s scope))
      }
  | Call
      { function :: !(Type s scope),
        argument :: !(Type s scope)
      }
  | Function
      { parameter :: !(Type s scope),
        result :: !(Type s scope)
      }
  | List
      { element :: !(Type s scope)
      }
  | LiftedList
      { items :: !(Strict.Vector1 (Type s scope))
      }
  | SmallType {}
  | Constraint {}

check :: Context s scope -> Unify.Type s scope -> Stage2.Type Position scope -> ST s (Type s scope)
check context@Context {localEnvironment, typeEnvironment} kind = \case
  Stage2.Variable {startPosition, variable} -> case localEnvironment Local.Table.! variable of
    LocalBinding.Rigid {rigid}
      | rigid <- lift rigid -> do
          Unify.unify context startPosition kind rigid
          pure $ Variable {variable}
    LocalBinding.Wobbly {wobbly} -> do
      Unify.unify context startPosition kind wobbly
      pure Variable {variable}
  Stage2.Constructor {constructorPosition, constructor} -> do
    kind' <- Builtin.kind (pure . lift) indexType indexConstructor constructor
    Unify.unify context constructorPosition kind kind'
    pure Constructor {constructor}
    where
      indexType constructor
        | TypeBinding {kind = kind'} <- typeEnvironment Type.Table.! constructor =
            do
              lift <$> kind'
      indexConstructor constructor = do
        let Constructor.Index {typeIndex, constructorIndex} = constructor
        datax <- do
          let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        DataInstance {types, constructors} <-
          Simple.Data.instanciate datax
        let root = Unify.constructor typeIndex
            base = foldl Unify.call root types
            ConstructorInstance {entries} =
              constructors Strict.Vector.! constructorIndex
        pure $ foldr Unify.function base entries
  Stage2.Tuple {startPosition, elements} -> do
    elements <- traverse (check context Unify.typex) elements
    Unify.unify context startPosition kind Unify.typex
    pure Tuple {elements}
  Stage2.Call {function, argument} -> do
    level <- Unify.fresh Unify.universe
    parameterType <- Unify.fresh (Unify.typeWith level)
    function <- check context (Unify.function parameterType kind) function
    argument <- check context parameterType argument
    pure Call {function, argument}
  Stage2.Function {parameter, operatorPosition, result} -> do
    level <- Unify.fresh Unify.universe
    level' <- Unify.fresh Unify.universe
    parameter <- check context (Unify.typeWith level) parameter
    result <- check context (Unify.typeWith level') result
    Unify.unify context operatorPosition (Unify.typeWith level') kind
    pure Function {parameter, result}
  Stage2.List {startPosition, element} -> do
    element <- check context Unify.typex element
    Unify.unify context startPosition kind Unify.typex
    pure List {element}
  Stage2.SmallType {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure SmallType {}
  Stage2.StrictFunction {operatorPosition} ->
    unsupportedFeatureStrictFunctions operatorPosition
  Stage2.LiftedList {startPosition, items} -> do
    inner <- Unify.fresh Unify.typex
    Unify.unify context startPosition kind (Unify.listWith inner)
    items <- traverse (check context inner) items
    pure LiftedList {items}
  Stage2.Type {startPosition, universe} -> case universe of
    Stage2.Small {} -> do
      Unify.unify context startPosition kind Unify.kind
      pure SmallType {}
    _ -> universeMustBeSmall startPosition
  Stage2.Constraint {startPosition} -> do
    Unify.unify context startPosition kind Unify.kind
    pure Constraint
  Stage2.Small {startPosition} -> uncheckable startPosition
  Stage2.Large {startPosition} -> uncheckable startPosition
  Stage2.Universe {startPosition} -> uncheckable startPosition

solve :: Synonym.Context s scope -> Type s scope -> ST s (Solved.Type scope)
solve context = \case
  Variable {variable} -> do
    pure
      Solved.Variable
        { variable
        }
  Constructor {constructor} -> do
    synonym <- Synonym.lookup context constructor
    pure
      Solved.Constructor
        { constructor,
          synonym
        }
  Tuple {elements} -> do
    elements <- traverse (solve context) elements
    pure $ Solved.Tuple {elements}
  Call {function, argument} -> do
    function <- solve context function
    argument <- solve context argument
    pure $ Solved.Call {function, argument}
  Function {parameter, result} -> do
    parameter <- solve context parameter
    result <- solve context result
    pure Solved.Function {parameter, result}
  List {element} -> do
    element <- solve context element
    pure Solved.List {element}
  LiftedList {items} -> do
    items <- traverse (solve context) items
    pure $ Solved.LiftedList {items}
  SmallType {} -> pure Solved.SmallType {}
  Constraint {} -> pure Solved.Constraint {}
