module Stage3.Tree.TypeDeclaration where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Stage1.Variable (ConstructorIdentifier)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDefinition as Stage2 (TypeDefinition (..))
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Temporary.TypeDefinition as Temporary.TypeDefinition
import Stage3.Tree.Type (Type)
import Stage3.Tree.TypeDefinition (TypeDefinition (..))
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type)

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope
  deriving (Show)

infix 4 :^

data TypeDeclaration scope
  = Inferred
      { name :: !ConstructorIdentifier,
        kind :: !(Simple.Type scope),
        definition :: !(TypeDefinition scope)
      }
  | Annotated
      { name :: !ConstructorIdentifier,
        kind :: !(Simple.Type scope),
        annotation :: !(Type scope),
        definition :: !(TypeDefinition scope)
      }
  deriving (Show)

strict declaration = name declaration :^ declaration

kind_ :: TypeDeclaration scope -> Simple.Type scope
kind_ = kind

check :: Context s scope -> Stage3.KindAnnotation scope -> Stage2.TypeDeclaration scope -> ST s (TypeDeclaration scope)
check _ KindAnnotation.Synonym {kind, annotation', definition, definition'} declaration
  | Stage2.Synonym {} <- Stage2.definition declaration = case annotation' of
      Strict.Nothing ->
        pure
          Inferred
            { name = Stage2.name declaration,
              kind,
              definition =
                Synonym
                  { definition,
                    definition'
                  }
            }
      Strict.Just annotation ->
        pure
          Annotated
            { name = Stage2.name declaration,
              kind,
              annotation,
              definition =
                Synonym
                  { definition,
                    definition'
                  }
            }
check context KindAnnotation.Inferred Stage2.Inferred {position, name, definition} = do
  kind <- Unify.fresh Unify.kind
  definition <- Temporary.TypeDefinition.check context kind definition
  kind <- Unify.solve position kind
  definition <- Temporary.TypeDefinition.solve context definition
  pure
    Inferred
      { name,
        kind,
        definition
      }
check context KindAnnotation.Annotation {annotation, kind} Stage2.Annotated {name, definition} = do
  definition <- Temporary.TypeDefinition.check context (Simple.Type.lift kind) definition
  definition <- Temporary.TypeDefinition.solve context definition
  pure
    Annotated
      { name,
        annotation,
        kind,
        definition
      }
check _ _ _ = error "bad type declaration check"
