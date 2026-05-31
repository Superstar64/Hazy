module Stage3.Temporary.TypeDeclarationExtra where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group, Normal)
import Stage2.Scope (Environment (..), Local)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.TypeDeclarationExtra as Solved
import qualified Stage2.Tree.TypeDeclarationExtra as Stage2
import qualified Stage2.Tree.TypeDefinition as TypeDefinition
import Stage2.Tree.TypeDefinition2 (TypeDefinition2 (..))
import Stage2.Tree.TypePattern (TypePattern (..))
import Stage3.Check.Context (Context)
import qualified Stage3.Check.Mask as Mask
import Stage3.Simple.SchemeOver (augment)
import Stage3.Temporary.MethodAbstract (MethodAbstract)
import qualified Stage3.Temporary.MethodAbstract as MethodAbstract
import Stage3.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import Stage3.Unify (Zonk (..))
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple (Constraint (..))
import qualified Stage4.Tree.Constraint as Simple.Constraint

data TypeDeclarationExtra s scope
  = ADT {position :: !Position}
  | Class
      { position :: !Position,
        methods :: !(Strict.Vector (MethodAbstract s (Local ':+ scope)))
      }
  | Synonym {position :: !Position}
  | GADT {position :: !Position}

instance Zonk TypeDeclarationExtra where
  zonk zonker = \case
    ADT {position} -> pure ADT {position}
    Class {position, methods} -> do
      methods <- traverse (Unify.zonk zonker) methods
      pure
        Class
          { position,
            methods
          }
    Synonym {position} -> pure Synonym {position}
    GADT {position} -> pure GADT {position}

check ::
  Context s scope ->
  Type.Index scope ->
  TypeDeclaration locality Normal Check scope ->
  Stage2.TypeDeclarationExtra Group Resolve scope ->
  ST s (TypeDeclarationExtra s scope)
check context classx declaration
  | definition <- TypeDeclaration.definition declaration = \case
      Stage2.ADT {position} -> pure ADT {position}
      Stage2.Synonym {position} -> pure Synonym {position}
      Stage2.GADT {position} -> pure GADT {position}
      Stage2.Class {position, methods} -> case definition of
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

solve :: TypeDeclarationExtra s scope -> ST s (Solved.TypeDeclarationExtra Group Check scope)
solve = \case
  ADT {position} -> pure Solved.ADT {position}
  Class {position, methods} -> do
    methods <- traverse MethodAbstract.solve methods
    pure Solved.Class {position, methods}
  Synonym {position} -> pure Solved.Synonym {position}
  GADT {position} -> pure Solved.GADT {position}
