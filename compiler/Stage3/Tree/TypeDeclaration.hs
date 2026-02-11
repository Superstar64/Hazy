module Stage3.Tree.TypeDeclaration where

import Control.Monad.ST (ST)
import Data.Foldable (Foldable (toList))
import Data.Functor.Identity (Identity (..))
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureGADTs)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Tree.Selector (Selector)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (TypePattern))
import qualified Stage2.Tree.TypePattern as Stage2.TypePattern
import Stage3.Check.Context (Context (..))
import Stage3.Check.KindAnnotation (KindAnnotation)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple (lift)
import qualified Stage3.Synonym as Synonym
import qualified Stage3.Temporary.Constraint as Constraint
import qualified Stage3.Temporary.Constructor as Unsolved.Constructor
import qualified Stage3.Temporary.Method as Unsolved.Method
import qualified Stage3.Temporary.Scheme as Unsolved.Scheme
import qualified Stage3.Temporary.TypePattern as Unsolved (TypePattern (TypePattern))
import qualified Stage3.Temporary.TypePattern as Unsolved.TypePattern
import Stage3.Tree.Constraint (Constraint)
import Stage3.Tree.Constructor (Constructor)
import Stage3.Tree.Method (Method)
import Stage3.Tree.Type (Type)
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type)

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope
  deriving (Show)

infix 4 :^

data TypeDeclaration scope
  = ADT
      { name :: !ConstructorIdentifier,
        kind :: !(Strict.Maybe (Type scope)),
        kind' :: !(Simple.Type scope),
        brand :: !Brand,
        parameters :: !(Strict.Vector (Simple.Type scope)),
        constructors :: !(Strict.Vector (Constructor (Scope.Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | Class
      { name :: !ConstructorIdentifier,
        kind :: !(Strict.Maybe (Type scope)),
        kind' :: !(Simple.Type scope),
        parameter :: !(Simple.Type scope),
        constraints :: !(Strict.Vector (Constraint scope)),
        methods :: !(Strict.Vector (Method (Scope.Local ':+ scope)))
      }
  | Synonym
      { name :: !ConstructorIdentifier,
        kind :: !(Strict.Maybe (Type scope)),
        kind' :: !(Simple.Type scope),
        definition :: !(Type (Scope.Local ':+ scope)),
        definition' :: !(Simple.Type (Scope.Local ':+ scope))
      }
  deriving (Show)

strict declaration = name declaration :^ declaration

kind'_ :: TypeDeclaration scope -> Simple.Type scope
kind'_ = kind'

checkHead ::
  (Traversable t) =>
  Context s scope ->
  Position ->
  KindAnnotation scope ->
  (t (Stage2.TypePattern Position), Unify.Type s scope) ->
  ST s (t (Unsolved.TypePattern s scope), Unify.Type s scope)
checkHead context position annotation (parameters, target) = do
  let fresh Stage2.TypePattern {name, position} = do
        level <- Unify.fresh Unify.universe
        typex <- Unify.fresh (Unify.typeWith level)
        pure
          Unsolved.TypePattern
            { name,
              typex,
              position
            }
  parameters <- traverse fresh parameters
  let kind = foldr (Unify.function . Unsolved.TypePattern.typex) target parameters
  case annotation of
    KindAnnotation.Inferred -> pure ()
    KindAnnotation.Annotation {kind'} -> do
      Unify.unify context position kind (Simple.lift kind')
    KindAnnotation.Synonym {} -> error "unexpected synonym annotation"
  pure (parameters, kind)

check :: Context s scope -> Stage3.KindAnnotation scope -> Stage2.TypeDeclaration scope -> ST s (TypeDeclaration scope)
check
  context
  annotation
  Stage2.ADT
    { position,
      name,
      brand,
      constructors,
      selectors,
      parameters
    } =
    do
      (parameters, kind) <- checkHead context position annotation (parameters, Unify.typex)
      context <- pure $ Unsolved.Scheme.augment parameters context
      constructors <- traverse (Unsolved.Constructor.check context) constructors
      let simplify = Synonym.fromProper context
      constructors <- traverse (Unsolved.Constructor.solve simplify) constructors
      kind' <- Unify.solve position kind
      let solveTypePattern = Unify.solve position . Unsolved.TypePattern.typex
      parameters <- Strict.Vector.fromList . toList <$> traverse solveTypePattern parameters
      pure $
        ADT
          { name,
            brand,
            constructors,
            kind',
            kind = Strict.Nothing,
            parameters,
            selectors
          }
check
  context
  annotation
  Stage2.Class
    { name,
      position,
      methods,
      constraints,
      parameter
    } = do
    (Identity parameter, kind) <-
      checkHead context position annotation (Identity parameter, Unify.constraint)
    context <- pure $ Unsolved.Scheme.augment (Strict.Vector.singleton parameter) context
    let simplify = Synonym.fromProper context
    constraints <- traverse (Constraint.check context) constraints
    constraints <- traverse (Constraint.solve simplify) constraints
    methods <- traverse (Unsolved.Method.check context) methods
    methods <- traverse (Unsolved.Method.solve simplify) methods
    kind' <- Unify.solve position kind
    parameter <- Unify.solve position . Unsolved.TypePattern.typex $ parameter
    pure $
      Class
        { name,
          kind',
          kind = Strict.Nothing,
          parameter,
          constraints,
          methods
        }
check _ annotation Stage2.Synonym {name}
  | KindAnnotation.Synonym
      { kind',
        kind,
        definition,
        definition'
      } <-
      annotation = do
      pure $
        Synonym
          { name,
            kind',
            kind,
            definition,
            definition'
          }
  | otherwise = error "bad annotation"
check _ _ Stage2.GADT {position} = unsupportedFeatureGADTs position
