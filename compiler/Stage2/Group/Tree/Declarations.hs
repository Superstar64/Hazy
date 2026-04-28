module Stage2.Group.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graph.StronglyConnected (Component, Index (..), tarjan)
import qualified Stage2.Group.Functor.Term.Declarations as Functor.Term
import qualified Stage2.Group.Functor.Type.Declarations as Functor.Type
import qualified Stage2.Group.Index.Link.Term as Term
import Stage2.Group.Temporary.Declaration (freeGroupTermVariables)
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Declaration (Declaration)
import qualified Stage2.Group.Tree.Declaration as Declartion
import Stage2.Group.Tree.Shared (Shared)
import qualified Stage2.Group.Tree.Shared as Shared
import Stage2.Group.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage2.Group.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage2.Index.Link.Term as Proper.Term
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Locality (Local)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Proper (Declarations (..))
import Stage2.Tree.Instance (Instance)
import Stage2.Tree.TypeDeclaration (freeGroupTypeVariables)
import qualified Stage2.Tree.TypeDeclaration as Proper (TypeDeclaration)
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

data Declarations locality scope = Declarations
  { terms :: !(Vector (Declaration locality scope)),
    types :: !(Vector (TypeDeclaration locality scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    shared :: !(Vector (Shared locality scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

group ::
  (Term.Link locality -> Temporary.Declaration locality scope) ->
  (Type.Link locality -> Proper.TypeDeclaration locality scope) ->
  Functor.Term.Declarations (Component (Term.Link locality)) ->
  Functor.Type.Declarations (Component (Type.Link locality)) ->
  Proper.Declarations locality scope ->
  Declarations locality scope
group
  indexTerm
  indexType
  Functor.Term.Declarations {terms = functorTerms, shared = functorShared}
  Functor.Type.Declarations {types = functorTypes}
  Proper.Declarations
    { terms,
      types,
      shared,
      typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Vector.zipWith (Declartion.group indexTerm) functorTerms terms,
        types = Vector.zipWith (TypeDeclaration.group indexType) functorTypes types,
        typeExtras,
        shared = Vector.zipWith (Shared.group indexTerm) functorShared shared,
        dataInstances,
        classInstances
      }

connect ::
  forall scope.
  Proper.Declarations Locality.Local (Scope.Declaration ':+ scope) ->
  Declarations Locality.Local (Scope.Declaration ':+ scope)
connect declarations@Proper.Declarations {terms, shared, types} =
  group indexTerm indexType termGroups typeGroups declarations
  where
    termGroups =
      tarjan
        Index {(!) = (Functor.Term.!)}
        (fmap Term.local . freeGroupTermVariables . indexTerm)
        termIndexes
    typeGroups =
      tarjan
        Index {(!) = (Functor.Type.!)}
        (fmap Type.local . freeGroupTypeVariables . indexType)
        typeIndexes
    termIndexes = Functor.Term.indexes Proper.Term.Declaration declarations
    typeIndexes = Functor.Type.indexes Type.Declaration declarations
    indexTerm ::
      Term.Link Local ->
      Temporary.Declaration Locality.Local (Scope.Declaration ':+ scope)
    indexTerm = \case
      Term.Link (Proper.Term.Declaration index) -> Temporary.Declaration $ terms Vector.! index
      Term.Share (Proper.Term.Declaration index) -> Temporary.Shared $ shared Vector.! index
    indexType ::
      Type.Link Local ->
      Proper.TypeDeclaration Locality.Local (Scope.Declaration ':+ scope)
    indexType = \case
      Type.Declaration index -> types Vector.! index
