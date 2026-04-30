{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declarations where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import Data.Maybe (fromJust, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graph.StronglyConnected (Index (..), tarjan)
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Extensions (Extensions (Extensions, stableImports))
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1 (toImport)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations (..))
import Stage2.FreeVariables (FreeTermVariables (..), FreeTypeVariables (..), Target (..))
import {-# SOURCE #-} qualified Stage2.Group.Functor.Term.Declarations as Functor.Term
import {-# SOURCE #-} qualified Stage2.Group.Functor.Type.Declarations as Functor.Type
import qualified Stage2.Group.Index.Link.Term as Group.Term
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Term0 as Term0 (Index (..))
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group, Normal)
import Stage2.Locality (Local)
import qualified Stage2.Resolve.Bindings as Bindings
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Resolve.Context as Context
import Stage2.Resolve.Import (StableImports (StableImports), pickImports)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.Declarations as Complete
import Stage2.Tree.Declaration (Declaration)
import qualified Stage2.Tree.Declaration as Declaration
import Stage2.Tree.Definition2 (freeGroupTermVariables)
import qualified Stage2.Tree.Definition2 as Definition2
import qualified Stage2.Tree.Definition3 as Definition3
import Stage2.Tree.Instance (Instance)
import Stage2.Tree.Shared (Shared (..))
import qualified Stage2.Tree.Shared as Shared
import Stage2.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage2.Tree.TypeDeclaration as TypeDeclaration
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import Stage2.Tree.TypeDefinition (TypeDefinition)
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2

data Declarations locality layout scope = Declarations
  { terms :: !(Vector (Declaration locality layout scope)),
    types :: !(Vector (TypeDeclaration locality layout scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    shared :: !(Vector (Shared locality layout scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

instance Shift (Declarations locality layout) where
  shift = shiftDefault

instance Shift.Functor (Declarations locality layout) where
  map
    category
    Declarations
      { terms,
        types,
        typeExtras,
        shared,
        dataInstances,
        classInstances
      } =
      Declarations
        { terms = fmap (Shift.map category) terms,
          types = fmap (Shift.map category) types,
          typeExtras = fmap (Shift.map category) typeExtras,
          shared = fmap (Shift.map category) shared,
          dataInstances = fmap (Shift.mapInstances category . fmap (Shift.map category)) dataInstances,
          classInstances = fmap (Shift.mapInstances category . fmap (Shift.map category)) classInstances
        }

instance FreeTermVariables (Declarations locality layout) where
  freeTermVariables target Declarations {terms, shared, typeExtras} =
    concat
      [ foldMap (freeTermVariables target) terms,
        foldMap (freeTermVariables target) shared,
        foldMap (freeTermVariables target) typeExtras
      ]

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Scope.Declaration ':+ scope),
    Declarations locality Normal (Scope.Declaration ':+ scope)
  )
resolve initial@Context {canonical, extensions} Stage1.Declarations {declarations} =
  (context, Complete.shrink complete)
  where
    context
      | context <- initial,
        Extensions {stableImports} <- extensions,
        imports <- pickImports (StableImports stableImports) (mapMaybe Stage1.toImport (toList declarations)) canonical,
        context <- imports Context.</> context,
        context@Context {locals} <- shift context,
        bindings <- Complete.bindings Term0.Declaration Type0.Declaration complete,
        context <- context {locals = bindings Bindings.</> locals} =
          context
    complete = runIdentity $ Complete.resolve context extensions (toList declarations)

group ::
  (Group.Term.Link locality -> Definition2.Auto scope) ->
  (Type.Link locality -> TypeDefinition scope) ->
  Functor.Term.Declarations (StronglyConnected.Component (Group.Term.Link locality)) ->
  Functor.Type.Declarations (StronglyConnected.Component (Type.Link locality)) ->
  Declarations locality Normal scope ->
  Declarations locality Group scope
group
  indexTerm
  indexType
  Functor.Term.Declarations {terms = functorTerms, shared = functorShared}
  Functor.Type.Declarations {types = functorTypes}
  Declarations
    { terms,
      types,
      shared,
      typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Vector.zipWith (Declaration.group indexTerm) functorTerms terms,
        types = Vector.zipWith (TypeDeclaration.group indexType) functorTypes types,
        typeExtras,
        shared = Vector.zipWith (Shared.group indexTerm) functorShared shared,
        dataInstances,
        classInstances
      }

connect ::
  forall scope.
  Declarations Local Normal (Scope.Declaration ':+ scope) ->
  Declarations Local Group (Scope.Declaration ':+ scope)
connect declarations@Declarations {terms, shared, types} =
  group indexTerm' indexType' termGroups typeGroups declarations
  where
    termIndexes = Functor.Term.indexes Term.Declaration declarations
    typeIndexes = Functor.Type.indexes Type.Declaration declarations

    termGroups =
      tarjan
        Index {(!) = (Functor.Term.!)}
        (fmap Group.Term.local . freeTerm . indexTerm)
        termIndexes
    typeGroups =
      tarjan
        Index {(!) = (Functor.Type.!)}
        (map Type.local . freeType . indexType)
        typeIndexes

    freeTerm = foldMap (freeGroupTermVariables Term0.Declaration)
    freeType = foldMap (freeTypeVariables Target)

    indexTerm' = fromJust . indexTerm
    indexType' = fromJust . indexType
    indexTerm :: Group.Term.Link Local -> Maybe (Definition2.Auto (Scope.Declaration ':+ scope))
    indexTerm = \case
      Group.Term.Link (Term.Declaration index) -> case terms Vector.! index of
        Declaration.Inferred {definition' = Definition3.Auto definition} ->
          Just $ Definition2.AnyAuto definition
        Declaration.Annotated {} -> Nothing
      Group.Term.Share (Term.Declaration index) -> case shared Vector.! index of
        Shared {definition = Definition3.Auto definition} ->
          Just $ Definition2.AnyAuto definition
    indexType :: Type.Link Local -> Maybe (TypeDefinition (Scope.Declaration ':+ scope))
    indexType = \case
      Type.Declaration index -> case types Vector.! index of
        TypeDeclaration.Inferred {definition' = TypeDefinition2.Auto definition} ->
          Just definition
        TypeDeclaration.Annotated {} -> Nothing
