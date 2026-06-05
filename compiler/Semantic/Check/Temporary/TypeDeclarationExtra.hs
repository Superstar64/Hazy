module Semantic.Check.Temporary.TypeDeclarationExtra where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraint as Simple (Constraint (..))
import qualified Core.Tree.Constraint as Simple.Constraint
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context)
import Semantic.Check.Go.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Check.Go.TypeDeclaration as TypeDeclaration
import qualified Semantic.Check.Mask as Mask
import Semantic.Check.Simple.SchemeOver (augment)
import Semantic.Check.Temporary.MethodAbstract (MethodAbstract)
import qualified Semantic.Check.Temporary.MethodAbstract as MethodAbstract
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group, Normal)
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic
import qualified Semantic.Tree.TypeDeclarationExtra as Solved
import qualified Semantic.Tree.TypeDefinition as TypeDefinition
import Semantic.Tree.TypeDefinition2 (TypeDefinition2 (..))
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data TypeDeclarationExtra s scope
  = ADT {position :: !Position}
  | Class
      { position :: !Position,
        methods :: !(Strict.Vector (MethodAbstract s (Local ':+ scope)))
      }
  | Synonym {position :: !Position}
  | GADT {position :: !Position}

check ::
  Context s scope ->
  Type.Index scope ->
  TypeDeclaration locality Normal Check scope ->
  Semantic.TypeDeclarationExtra Group Resolve scope ->
  ST s (TypeDeclarationExtra s scope)
check context classx declaration
  | definition <- TypeDeclaration.definition declaration = \case
      Semantic.ADT {position} -> pure ADT {position}
      Semantic.Synonym {position} -> pure Synonym {position}
      Semantic.GADT {position} -> pure GADT {position}
      Semantic.Class {position, methods} -> case definition of
        _ ::: TypeDefinition.Class {parameter = TypePattern {typex = Solved parameter}, methods = base} -> do
          context <-
            augment
              position
              (Strict.Vector.singleton parameter)
              ( Strict.Vector.singleton
                  Simple.Constraint
                    { classx = Type2.Index classx,
                      head = 0,
                      arguments = Strict.Vector.empty
                    }
              )
              Mask.Inline
              context
          methods <- sequence $ Strict.Vector.zipWith (MethodAbstract.check context position) base methods
          pure Class {position, methods}
        _ -> error "bad proper"

solve :: TypeDeclarationExtra s scope -> Unify.Solve s (Solved.TypeDeclarationExtra Group Check scope)
solve = \case
  ADT {position} -> pure Solved.ADT {position}
  Class {position, methods} -> do
    methods <- traverse MethodAbstract.solve methods
    pure Solved.Class {position, methods}
  Synonym {position} -> pure Solved.Synonym {position}
  GADT {position} -> pure Solved.GADT {position}
