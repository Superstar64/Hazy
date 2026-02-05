module Stage3.Temporary.Constraint where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import Data.List.Reverse (List (Nil, (:>)))
import qualified Data.List.Reverse as Reverse
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import Stage2.Index.Local (Index (Local))
import qualified Stage2.Index.Table.Local as Table.Local
import qualified Stage2.Index.Table.Type as Type.Table
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Constraint as Stage2
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import qualified Stage3.Check.LocalBinding as LocalBinding
import Stage3.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Simple.Type (lift)
import qualified Stage3.Simple.Type as Simple (lift)
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Type (Type)
import qualified Stage3.Temporary.Type as Type (check, solve)
import qualified Stage3.Tree.Constraint as Solved
import qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} Stage4.Tree.TypeDeclaration (assumeData)
import Prelude hiding (head)

data Constraint s scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type s (Local ':+ scope)))
  }

check ::
  Context s (Local ':+ scope) ->
  Stage2.Constraint Position scope ->
  ST s (Constraint s scope)
check
  context@Context {localEnvironment, typeEnvironment}
  Stage2.Constraint
    { startPosition,
      classx,
      head,
      arguments
    } = do
    target <- Unify.fresh Unify.kind
    let indexType constructor
          | TypeBinding {kind = kind'} <- typeEnvironment Type.Table.! constructor =
              do
                lift <$> kind'
        indexConstructor constructor = do
          let Constructor.Index {typeIndex, constructorIndex} = constructor
          datax <- do
            let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.Table.! index)
            Builtin.index pure get typeIndex
          DataInstance {types, constructors} <-
            Simple.Data.instanciate datax
          let root = Unify.constructor typeIndex
              base = foldl Unify.call root types
              ConstructorInstance {entries} =
                constructors Strict.Vector.! constructorIndex
          pure $ foldr Unify.function base entries
    real <- Builtin.kind (pure . lift) indexType indexConstructor (shift classx)

    Unify.unify context startPosition (Unify.function target Unify.constraint) real

    let check context kind (arguments :> argument) = do
          parameterType <- Unify.fresh Unify.kind
          arguments <- check context (Unify.function parameterType kind) arguments
          argument <- Type.check context parameterType argument
          pure (arguments :> argument)
        check context kind Nil = case localEnvironment Table.Local.! Local head of
          LocalBinding.Wobbly {wobbly} -> do
            Unify.unify context startPosition kind wobbly
            pure Nil
          LocalBinding.Rigid {rigid} -> do
            Unify.unify context startPosition kind (Simple.lift rigid)
            pure Nil

    arguments <- Strict.Vector.fromList . toList <$> check context target (Reverse.fromList $ toList arguments)

    pure $ Constraint {classx, head, arguments}

solve :: Synonym.Context s (Local ':+ scope) -> Constraint s scope -> ST s (Solved.Constraint scope)
solve context Constraint {classx, head, arguments} = do
  arguments <- traverse (Type.solve context) arguments
  pure
    Solved.Constraint
      { classx,
        head,
        arguments
      }
