module Semantic.Check.Temporary.Constraint where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Builtin as Builtin
import {-# SOURCE #-} Core.Tree.TypeDeclaration (assumeData)
import Data.Foldable (toList)
import Data.List.Reverse (List (Nil, (:>)))
import qualified Data.List.Reverse as Reverse
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.LocalBinding as LocalBinding
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Simple.Type (lift)
import qualified Semantic.Check.Simple.Type as Simple (lift)
import Semantic.Check.Temporary.Type (Type)
import qualified Semantic.Check.Temporary.Type as Type (check, solve)
import Semantic.Check.TypeBinding (TypeBinding (TypeBinding))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Index.Local (Index (Local))
import qualified Semantic.Index.Table.Local as Table.Local
import qualified Semantic.Index.Table.Type as Type.Table
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constraint as Semantic
import qualified Semantic.Tree.Constraint as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (head)

data Constraint s scope = Constraint
  { startPosition :: !Position,
    classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type s (Local ':+ scope)))
  }

check ::
  Context s (Local ':+ scope) ->
  Semantic.Constraint Position Resolve scope ->
  ST s (Constraint s scope)
check
  context@Context {localEnvironment, typeEnvironment}
  Semantic.Constraint
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
