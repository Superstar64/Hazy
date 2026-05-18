module Stage3.Temporary.TypeDeclarationExtra where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Normal)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.Method (Method (..))
import qualified Stage2.Tree.TypeDeclarationExtra as Stage2
import qualified Stage2.Tree.TypeDefinition as TypeDefinition
import Stage2.Tree.TypePattern (TypePattern (..))
import Stage3.Check.Context (Context)
import qualified Stage3.Check.Mask as Mask
import Stage3.Simple.SchemeOver (augment, augment')
import qualified Stage3.Simple.Type as Simple.Type
import Stage3.Temporary.Definition (Definition)
import {-# SOURCE #-} qualified Stage3.Temporary.Definition as Definition
import Stage3.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage3.Tree.TypeDeclarationExtra as Solved
import Stage3.Tree.TypeDefinition2 (TypeDefinition2 (..))
import Stage3.Unify (Zonk (..))
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple (Constraint (..))
import qualified Stage4.Tree.Constraint as Simple.Constraint
import qualified Stage4.Tree.Scheme as Simple (Scheme (..))
import qualified Stage4.Tree.Scheme as Simple.Scheme
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))

data TypeDeclarationExtra s scope
  = ADT
  | Class
      { defaults :: !(Strict.Vector (Strict.Maybe (Definition s (Local ':+ Local ':+ scope))))
      }
  | Synonym
  | GADT

instance Zonk TypeDeclarationExtra where
  zonk zonker = \case
    ADT -> pure ADT
    Class {defaults} -> do
      defaults <- traverse (traverse (Unify.zonk zonker)) defaults
      pure
        Class
          { defaults
          }
    Synonym -> pure Synonym
    GADT -> pure GADT

check ::
  Context s scope ->
  Type.Index scope ->
  TypeDeclaration scope ->
  Stage2.TypeDeclarationExtra Normal scope ->
  ST s (TypeDeclarationExtra s scope)
check context classx declaration
  | definition <- TypeDeclaration.definition declaration = \case
      Stage2.ADT {} -> pure ADT
      Stage2.Synonym {} -> pure Synonym
      Stage2.GADT {} -> pure GADT
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
          let go
                Method {annotation}
                definition
                  | Simple.Scheme scheme@Simple.SchemeOver {result} <-
                      Simple.Scheme.simplify annotation = do
                      for definition $ \definition -> do
                        context <- augment' position scheme Mask.Runtime context
                        Definition.check context (Simple.Type.lift result) (shift definition)
          defaults <- sequence $ Strict.Vector.zipWith go base methods
          pure Class {defaults}
        _ -> error "bad proper"

solve :: TypeDeclarationExtra s scope -> ST s (Solved.TypeDeclarationExtra scope)
solve = \case
  ADT -> pure Solved.ADT
  Class {defaults} -> do
    defaults <- traverse (traverse Definition.solve) defaults
    pure Solved.Class {defaults}
  Synonym {} -> pure Solved.Synonym
  GADT -> pure Solved.GADT
