module Stage3.Tree.TypeDeclaration (TypeDeclaration (..), kind', check, lazy) where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector as Vector
import Data.Vector.Strict (toLazy)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Variable (QualifiedConstructorIdentifier (..), Qualifiers)
import qualified Stage2.Index.Link.Type as Type
import Stage2.Layout (Group)
import Stage2.Shift (Category (Shift), shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.TypeDeclaration (TypeDeclaration (..), kind', lazy)
import Stage2.Tree.TypeDefinition (TypeDefinition (..))
import Stage2.Tree.TypeDefinition2 (Annotation (..), Element (..), Set (..), TypeDefinition2 (..))
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import Stage3.Check.Context (Context (..), groupTypeBindings)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Temporary.TypeDefinition as Temporary.TypeDefinition
import qualified Stage3.Temporary.TypeDefinition as TypeDefinition
import qualified Stage3.Temporary.TypeDefinition2 as Temporary
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple

check ::
  Context s scope ->
  (Type.Link locality -> Int -> ST s (Simple.Type scope)) ->
  Qualifiers ->
  Type.Link locality ->
  Stage3.KindAnnotation scope ->
  TypeDeclaration locality Group Resolve scope ->
  ST s (TypeDeclaration locality Group Check scope)
check
  context
  linked
  qualifiers
  link
  annotation
  TypeDeclaration {position, name, constructorNames, definition}
    | KindAnnotation.Synonym {kind, annotation', parameters, synonym} <- annotation = case annotation' of
        Strict.Nothing ->
          let element =
                Element
                  { element =
                      Synonym
                        { parameters = Shift.map Shift <$> parameters,
                          synonym = Shift.map (Shift.Over Shift) synonym
                        },
                    typex = Solved kind,
                    position,
                    name = qualifiers :=. name,
                    constructorNames = Strict.Vector.empty,
                    link
                  }
           in pure
                TypeDeclaration
                  { position,
                    name,
                    constructorNames,
                    definition = Group $ Set $ Strict.Vector.singleton element,
                    kind = Solved kind
                  }
        Strict.Just annotation ->
          pure
            TypeDeclaration
              { position,
                name,
                constructorNames,
                definition = Annotated annotation ::: Synonym {parameters, synonym},
                kind = Solved kind
              }
    | otherwise = case definition of
        Annotated {} ::: definition
          | KindAnnotation.Annotation {annotation, kind} <- annotation -> do
              definition <- Temporary.TypeDefinition.check context (Simple.Type.lift kind) definition
              definition <- Temporary.TypeDefinition.solve context definition
              pure
                TypeDeclaration
                  { position,
                    name,
                    constructorNames,
                    definition = Annotated annotation ::: definition,
                    kind = Solved kind
                  }
          | otherwise -> error "bad annotation"
        Group (Set set) -> do
          fresh <- Vector.replicateM (length set) (Unify.fresh Unify.kind)
          let context' =
                groupTypeBindings
                  (TypeDefinition2.position <$> toLazy set)
                  (TypeDefinition2.label <$> toLazy set)
                  fresh
                  context
          set <-
            flip Strict.Vector.imapM set $
              \index
               Element {element, position, name, constructorNames, link} -> do
                  let typex = fresh Vector.! index
                  element <- TypeDefinition.check context' (shift typex) element
                  pure $ Temporary.Element {element, typex, position, name, constructorNames, link}
          set <- traverse (Temporary.solveElement context') set
          kind <- Unify.solve position (Vector.head fresh)
          pure
            TypeDeclaration
              { position,
                name,
                constructorNames,
                definition = Group $ Set set,
                kind = Solved kind
              }
        Link link id -> do
          kind <- linked link id
          pure
            TypeDeclaration
              { position,
                name,
                constructorNames,
                definition = Link link id,
                kind = Solved kind
              }
