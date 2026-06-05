module Stage2.Check.Temporary.Constraint where

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
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Constraint as Solved
import qualified Stage2.Tree.Constraint as Stage2
import Stage2.Check.Context (Context (..))
import qualified Stage2.Check.DataInstance as DataInstance
import qualified Stage2.Check.LocalBinding as LocalBinding
import Stage2.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Stage2.Check.TypeBinding as TypeBinding
import qualified Stage2.Check.Simple.Data as Simple.Data
import Stage2.Check.Simple.Type (lift)
import qualified Stage2.Check.Simple.Type as Simple (lift)
import Stage2.Check.Temporary.Type (Type)
import qualified Stage2.Check.Temporary.Type as Type (check, solve)
import qualified Stage2.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} Stage4.Tree.TypeDeclaration (assumeData)
import Prelude hiding (head)

data Constraint s scope = Constraint
  { startPosition :: !Position,
    classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type s (Local ':+ scope)))
  }

check ::
  Context s (Local ':+ scope) ->
  Stage2.Constraint Position Resolve scope ->
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
          | TypeBinding {kind} <- typeEnvironment Type.Table.! constructor =
              do
                kind <- kind
                case kind of
                  TypeBinding.Rigid kind -> pure $ lift kind
                  TypeBinding.Wobbly kind -> pure kind
        indexLift constructor@Constructor.Index {typeIndex} = do
          datax <- do
            let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.Table.! index)
            datax <- Builtin.index pure get typeIndex
            Simple.Data.instanciate context startPosition datax
          pure $ DataInstance.constructorFunction datax constructor
    real <- Builtin.kind (pure . lift) indexType indexLift (shift classx)

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

    pure $ Constraint {startPosition, classx, head, arguments}

solve :: Context s (Local ':+ scope) -> Constraint s scope -> Unify.Solve s (Solved.Constraint Position Check scope)
solve context Constraint {startPosition, classx, head, arguments} = do
  arguments <- traverse (Type.solve context) arguments
  pure
    Solved.Constraint
      { startPosition,
        classx,
        head,
        arguments
      }
