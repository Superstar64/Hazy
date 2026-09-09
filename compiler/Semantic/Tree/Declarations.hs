{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Declarations where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.NaturalTransformation (NaturalTransformation (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Graph.StronglyConnected (Index (..), tarjan)
import qualified Graph.StronglyConnected as StronglyConnected
import Semantic.Connect (Connect)
import qualified Semantic.Connect as Connect
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Functor2 (Traversable2 (..))
import {-# SOURCE #-} qualified Semantic.Group.Functor.Term.Declarations as Functor.Term
import {-# SOURCE #-} qualified Semantic.Group.Functor.Type.Declarations as Functor.Type
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Index.Term0 as Term0 (Index (..))
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group, Normal)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit)
import Semantic.Tree.Declaration (Declaration (..))
import qualified Semantic.Tree.Declaration as Declaration
import qualified Semantic.Tree.Definition4 as Definition4
import qualified Semantic.Tree.Group as Group
import Semantic.Tree.Instance (Instance)
import Semantic.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Tree.TypeDeclaration as TypeDeclaration
import Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Tree.TypeGroup as TypeGroup
import Syntax.Variable (Qualifiers)
import qualified Syntax.Variable as Variable

data Declarations solve logical locality loeb layout stage scope = Declarations
  { terms :: !(Vector (Declaration solve logical locality loeb layout stage scope)),
    types :: !(Vector (TypeDeclaration locality loeb layout stage scope)),
    typeExtras :: !(Vector (loeb (solve (TypeDeclarationExtra layout stage scope)))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance solve loeb layout stage scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance solve loeb layout stage scope)))
  }
  deriving (Show)

instance (Functor solve, Functor loeb) => Shift0.Functor (Declarations solve logical locality loeb layout stage) where
  map = Shift.mapDefault

instance (Functor solve, Functor loeb) => Shift.Functor (Declarations solve logical locality loeb layout stage) where
  map
    category
    Declarations
      { terms,
        types,
        typeExtras,
        dataInstances,
        classInstances
      } =
      Declarations
        { terms = fmap (Shift.map category) terms,
          types = fmap (Shift.map category) types,
          typeExtras = fmap (fmap $ Shift.map category) <$> typeExtras,
          dataInstances = fmap (Shift.mapInstances category . fmap (Shift.map category)) dataInstances,
          classInstances = fmap (Shift.mapInstances category . fmap (Shift.map category)) classInstances
        }

instance (Foldable solve, Foldable loeb) => FreeTermVariables (Declarations solve logical locality loeb) where
  freeTermVariables target Declarations {terms, typeExtras} =
    concat
      [ foldMap (freeTermVariables target) terms,
        foldMap (foldMap $ foldMap $ freeTermVariables target) typeExtras
      ]

instance Traversable2 (Declarations solve logical locality) where
  traverse2
    (Morph f)
    Declarations
      { terms,
        types,
        typeExtras,
        dataInstances,
        classInstances
      } =
      declarations
        <$> traverse (traverse2 (Morph f)) terms
        <*> traverse (traverse2 (Morph f)) types
        <*> traverse (getCompose . f) typeExtras
        <*> traverse (traverse (traverse2 (Morph f))) dataInstances
        <*> traverse (traverse (traverse2 (Morph f))) classInstances
      where
        declarations terms types typeExtras dataInstances classInstances =
          Declarations {terms, types, typeExtras, dataInstances, classInstances}

group ::
  Qualifiers ->
  (Term0.Index scope -> Term.Link locality) ->
  (Type0.Index scope -> Type.Link locality) ->
  (Term.Link locality -> Declaration.Groupable scope) ->
  (Type.Link locality -> TypeDeclaration.Groupable scope) ->
  Functor.Term.Declarations (StronglyConnected.Component (Term.Link locality)) ->
  Functor.Type.Declarations (StronglyConnected.Component (Type.Link locality)) ->
  Declarations Identity Void locality Identity Normal Resolve scope ->
  Declarations Identity Void locality Identity Group Resolve scope
group
  qualifiers
  linkTerm
  linkType
  indexTerm
  indexType
  Functor.Term.Declarations {terms = functorTerms}
  Functor.Type.Declarations {types = functorTypes}
  Declarations
    { terms,
      types,
      typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Vector.zipWith (Declaration.group linkTerm indexTerm) functorTerms terms,
        types = Vector.zipWith (TypeDeclaration.group qualifiers linkType indexType) functorTypes types,
        typeExtras = fmap (fmap Connect.connect) <$> typeExtras,
        dataInstances = fmap Connect.connect <$> dataInstances,
        classInstances = fmap Connect.connect <$> classInstances
      }

ungroup ::
  (Term.Link locality -> Term0.Index scope) ->
  (Type.Link locality -> Type0.Index scope) ->
  (Term.Link locality -> Implicit (Group.Set locality) Group Check scope) ->
  (Type.Link locality -> TypeGroup.Set locality Check scope) ->
  Declarations Identity Void locality Identity Group Check scope ->
  Declarations Identity Void locality Identity Normal Check scope
ungroup
  indexTerm
  indexType
  lookupTerm
  lookupType
  Declarations
    { terms,
      types,
      typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Declaration.ungroup indexTerm lookupTerm <$> terms,
        types = TypeDeclaration.ungroup indexType lookupType <$> types,
        typeExtras = fmap (fmap Connect.seperate) <$> typeExtras,
        dataInstances = fmap Connect.seperate <$> dataInstances,
        classInstances = fmap Connect.seperate <$> classInstances
      }

connect ::
  forall scope.
  Declarations Identity Void Locality.Local Identity Normal Resolve (Scope.Declaration ':+ scope) ->
  Declarations Identity Void Locality.Local Identity Group Resolve (Scope.Declaration ':+ scope)
connect declarations@Declarations {terms, types} =
  group Variable.Local Term.local Type.local indexTerm' indexType' termGroups typeGroups declarations
  where
    termIndexes = Functor.Term.indexes Term.Declaration declarations
    typeIndexes = Functor.Type.indexes Type.Declaration declarations

    termGroups =
      tarjan
        Index {(!) = (Functor.Term.!)}
        (fmap Term.local . freeTerm . indexTerm)
        termIndexes
    typeGroups =
      tarjan
        Index {(!) = (Functor.Type.!)}
        (map Type.local . freeType . indexType)
        typeIndexes

    freeTerm = foldMap Declaration.groupFree
    freeType = foldMap TypeDeclaration.groupFree

    indexTerm' = fromJust . indexTerm
    indexType' = fromJust . indexType
    indexTerm (Term.Declaration index) = Declaration.groupable (terms Vector.! index)
    indexType (Type.Declaration index) = TypeDeclaration.groupable (types Vector.! index)

seperate ::
  forall scope.
  Declarations Identity Void Locality.Local Identity Group Check (Scope.Declaration ':+ scope) ->
  Declarations Identity Void Locality.Local Identity Normal Check (Scope.Declaration ':+ scope)
seperate declarations@Declarations {terms, types} =
  ungroup Term.unlocal Type.unlocal lookupTerm lookupType declarations
  where
    lookupTerm ::
      Term.Link Locality.Local ->
      Implicit (Group.Set Locality.Local) Group Check (Scope.Declaration ':+ scope)
    lookupTerm = \case
      Term.Declaration index
        | Declaration {definition} <- terms Vector.! index,
          Definition4.Group (Identity (_ Group.:::: Identity set)) <- definition ->
            set
      _ -> error "bad term lookup"
    lookupType ::
      Type.Link Locality.Local ->
      TypeGroup.Set Locality.Local Check (Scope.Declaration ':+ scope)
    lookupType = \case
      Type.Declaration index
        | TypeDeclaration {definition} <- types Vector.! index,
          TypeDefinition2.Group (Identity (_ TypeGroup.:::: set)) <- definition ->
            set
      _ -> error "bad type lookup"

newtype Local layout stage scope
  = Local (Declarations Identity Void Locality.Local Identity layout stage (Scope.Declaration ':+ scope))
  deriving (Show)

instance Shift0.Functor (Local layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Local layout stage) where
  map category (Local declarations) = Local (Shift.map (Shift.Over category) declarations)

instance FreeTermVariables Local where
  freeTermVariables target (Local declarations) =
    freeTermVariables (FreeVariables.Over target) declarations

instance Connect Local where
  connect (Local declarations) = Local (connect declarations)
  seperate (Local declarations) = Local (seperate declarations)
