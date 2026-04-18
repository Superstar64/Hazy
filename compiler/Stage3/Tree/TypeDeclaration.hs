module Stage3.Tree.TypeDeclaration where

import Control.Monad.ST (ST)
import Data.Foldable (Foldable (toList))
import Data.Functor.Identity (Identity (..))
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureGADTs)
import Stage1.Position (Position)
import Stage1.Variable (ConstructorIdentifier)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDefinition as Stage2 (TypeDefinition (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (TypePattern))
import qualified Stage2.Tree.TypePattern as Stage2.TypePattern
import Stage3.Check.Context (Context (..))
import Stage3.Check.KindAnnotation (KindAnnotation)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple (lift)
import qualified Stage3.Temporary.Constraint as Constraint
import qualified Stage3.Temporary.Constructor as Unsolved.Constructor
import qualified Stage3.Temporary.Method as Unsolved.Method
import qualified Stage3.Temporary.Scheme as Unsolved.Scheme
import qualified Stage3.Temporary.TypePattern as Unsolved (TypePattern (TypePattern))
import qualified Stage3.Temporary.TypePattern as Unsolved.TypePattern
import Stage3.Tree.Type (Type)
import Stage3.Tree.TypeDefinition (TypeDefinition (..))
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type)

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope
  deriving (Show)

infix 4 :^

data TypeDeclaration scope
  = TypeDeclaration
  { name :: !ConstructorIdentifier,
    kind :: !(Strict.Maybe (Type scope)),
    kind' :: !(Simple.Type scope),
    definition :: !(TypeDefinition scope)
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
  Stage2.TypeDeclaration
    { position,
      name,
      definition =
        Stage2.ADT
          { brand,
            constructors,
            selectors,
            parameters
          }
    } =
    do
      (parameters, kind) <- checkHead context position annotation (parameters, Unify.typex)
      context <- pure $ Unsolved.Scheme.augment parameters context
      constructors <- traverse (Unsolved.Constructor.check context) constructors
      constructors <- traverse (Unsolved.Constructor.solve context) constructors
      kind' <- Unify.solve position kind
      let solveTypePattern = Unify.solve position . Unsolved.TypePattern.typex
      parameters <- Strict.Vector.fromList . toList <$> traverse solveTypePattern parameters
      pure $
        TypeDeclaration
          { name,
            kind',
            kind = Strict.Nothing,
            definition =
              ADT
                { brand,
                  constructors,
                  parameters,
                  selectors
                }
          }
check
  context
  annotation
  Stage2.TypeDeclaration
    { name,
      position,
      definition =
        Stage2.Class
          { methods,
            constraints,
            parameter
          }
    } = do
    (Identity parameter, kind) <-
      checkHead context position annotation (Identity parameter, Unify.constraint)
    context <- pure $ Unsolved.Scheme.augment (Strict.Vector.singleton parameter) context
    constraints <- traverse (Constraint.check context) constraints
    constraints <- traverse (Constraint.solve context) constraints
    methods <- traverse (Unsolved.Method.check context) methods
    methods <- traverse (Unsolved.Method.solve context) methods
    kind' <- Unify.solve position kind
    parameter <- Unify.solve position . Unsolved.TypePattern.typex $ parameter
    pure $
      TypeDeclaration
        { name,
          kind',
          kind = Strict.Nothing,
          definition =
            Class
              { parameter,
                constraints,
                methods
              }
        }
check _ annotation Stage2.TypeDeclaration {name, definition = Stage2.Synonym {}}
  | KindAnnotation.Synonym
      { kind',
        kind,
        definition,
        definition'
      } <-
      annotation = do
      pure $
        TypeDeclaration
          { name,
            kind',
            kind,
            definition =
              Synonym
                { definition,
                  definition'
                }
          }
  | otherwise = error "bad annotation"
check _ _ Stage2.TypeDeclaration {position, definition = Stage2.GADT {}} = unsupportedFeatureGADTs position
