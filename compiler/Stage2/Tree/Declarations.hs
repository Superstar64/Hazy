{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declarations where

import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graph.StronglyConnected (Index (..), tarjan)
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Variable (Qualifiers (Local))
import qualified Stage2.Connect as Connect
import Stage2.FreeVariables (FreeTermVariables (..))
import {-# SOURCE #-} qualified Stage2.Group.Functor.Term.Declarations as Functor.Term
import {-# SOURCE #-} qualified Stage2.Group.Functor.Type.Declarations as Functor.Type
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Term0 as Term0 (Index (..))
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group, Normal)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Implicit (Implicit)
import Stage2.Tree.Declaration (Declaration (..))
import qualified Stage2.Tree.Declaration as Declaration
import qualified Stage2.Tree.Definition4 as Definition4
import Stage2.Tree.Instance (Instance)
import Stage2.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDeclaration as TypeDeclaration
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2

data Declarations locality layout stage scope = Declarations
  { terms :: !(Vector (Declaration locality layout stage scope)),
    types :: !(Vector (TypeDeclaration locality layout stage scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra layout stage scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance layout stage scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance layout stage scope)))
  }
  deriving (Show)

instance Shift (Declarations locality layout stage) where
  shift = shiftDefault

instance Shift.Functor (Declarations locality layout stage) where
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
          typeExtras = fmap (Shift.map category) typeExtras,
          dataInstances = fmap (Shift.mapInstances category . fmap (Shift.map category)) dataInstances,
          classInstances = fmap (Shift.mapInstances category . fmap (Shift.map category)) classInstances
        }

instance FreeTermVariables (Declarations locality layout) where
  freeTermVariables target Declarations {terms, typeExtras} =
    concat
      [ foldMap (freeTermVariables target) terms,
        foldMap (freeTermVariables target) typeExtras
      ]

group ::
  Qualifiers ->
  (Term0.Index scope -> Term.Link locality) ->
  (Type0.Index scope -> Type.Link locality) ->
  (Term.Link locality -> Declaration.Groupable scope) ->
  (Type.Link locality -> TypeDeclaration.Groupable scope) ->
  Functor.Term.Declarations (StronglyConnected.Component (Term.Link locality)) ->
  Functor.Type.Declarations (StronglyConnected.Component (Type.Link locality)) ->
  Declarations locality Normal Resolve scope ->
  Declarations locality Group Resolve scope
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
        typeExtras = Connect.connect <$> typeExtras,
        dataInstances = fmap Connect.connect <$> dataInstances,
        classInstances = fmap Connect.connect <$> classInstances
      }

ungroup ::
  (Term.Link locality -> Term0.Index scope) ->
  (Type.Link locality -> Type0.Index scope) ->
  (Term.Link locality -> Implicit (Definition4.Set locality Check) Check scope) ->
  (Type.Link locality -> TypeDefinition2.Set locality Check scope) ->
  Declarations locality Group Check scope ->
  Declarations locality Normal Check scope
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
        typeExtras = Connect.seperate <$> typeExtras,
        dataInstances = fmap Connect.seperate <$> dataInstances,
        classInstances = fmap Connect.seperate <$> classInstances
      }

connect ::
  forall scope.
  Declarations Local Normal Resolve (Scope.Declaration ':+ scope) ->
  Declarations Local Group Resolve (Scope.Declaration ':+ scope)
connect declarations@Declarations {terms, types} =
  group Local Term.local Type.local indexTerm' indexType' termGroups typeGroups declarations
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
  Declarations Local Group Check (Scope.Declaration ':+ scope) ->
  Declarations Local Normal Check (Scope.Declaration ':+ scope)
seperate declarations@Declarations {terms, types} =
  ungroup Term.unlocal Type.unlocal lookupTerm lookupType declarations
  where
    lookupTerm :: Term.Link Local -> Implicit (Definition4.Set Local Check) Check (Scope.Declaration ':+ scope)
    lookupTerm = \case
      Term.Declaration index
        | Declaration {definition} <- terms Vector.! index,
          _ Definition4.:::: set <- definition ->
            set
      _ -> error "bad term lookup"
    lookupType :: Type.Link Local -> TypeDefinition2.Set Local Check (Scope.Declaration ':+ scope)
    lookupType = \case
      Type.Declaration index
        | TypeDeclaration {definition} <- types Vector.! index,
          _ TypeDefinition2.:::: set <- definition ->
            set
      _ -> error "bad type lookup"
