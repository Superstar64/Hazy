module Stage2.Group.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graph.StronglyConnected (Component, Index (..), tarjan)
import qualified Stage2.Group.Functor.Term.Declarations as Functor.Term
import qualified Stage2.Group.Functor.Type.Declarations as Functor.Type
import qualified Stage2.Group.Index.Term0 as Term0
import Stage2.Group.Temporary.Declaration (freeGroupTermVariables)
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Declaration (Declaration)
import qualified Stage2.Group.Tree.Declaration as Declartion
import Stage2.Group.Tree.Group (Group)
import qualified Stage2.Group.Tree.Group as Group
import Stage2.Group.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage2.Group.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage2.Index.Term0 as Proper.Term0
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Proper (Declarations (..))
import Stage2.Tree.Instance (Instance)
import Stage2.Tree.TypeDeclaration (freeGroupTypeVariables)
import qualified Stage2.Tree.TypeDeclaration as Proper (TypeDeclaration)
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

data Declarations scope = Declarations
  { terms :: !(Vector (Declaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    shared :: !(Vector (Group scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

group ::
  (Term0.Index scope -> Temporary.Declaration scope) ->
  (Type0.Index scope -> Proper.TypeDeclaration scope) ->
  Functor.Term.Declarations (Component (Term0.Index scope)) ->
  Functor.Type.Declarations (Component (Type0.Index scope)) ->
  Proper.Declarations scope ->
  Declarations scope
group
  indexTerm
  indexType
  Functor.Term.Declarations {terms, shared}
  Functor.Type.Declarations {types}
  Proper.Declarations
    { typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Declartion.group indexTerm <$> terms,
        types = TypeDeclaration.group indexType <$> types,
        typeExtras,
        shared = Group.group indexTerm <$> shared,
        dataInstances,
        classInstances
      }

connect ::
  forall scope.
  Proper.Declarations (Scope.Declaration ':+ scope) ->
  Declarations (Scope.Declaration ':+ scope)
connect declarations@Proper.Declarations {terms, shared, types} =
  group indexTerm indexType termGroups typeGroups declarations
  where
    termGroups = tarjan Index {(!) = (Functor.Term.!)} (freeGroupTermVariables . indexTerm) termIndexes
    typeGroups = tarjan Index {(!) = (Functor.Type.!)} (freeGroupTypeVariables . indexType) typeIndexes
    termIndexes = Functor.Term.indexes Proper.Term0.Declaration declarations
    typeIndexes = Functor.Type.indexes Type0.Declaration declarations
    indexTerm ::
      Term0.Index (Scope.Declaration ':+ scope) ->
      Temporary.Declaration (Scope.Declaration ':+ scope)
    indexTerm = \case
      Term0.Index (Proper.Term0.Declaration index) -> Temporary.Declaration $ terms Vector.! index
      Term0.Share (Proper.Term0.Declaration index) -> Temporary.Shared $ shared Vector.! index
    indexType ::
      Type0.Index (Scope.Declaration ':+ scope) ->
      Proper.TypeDeclaration (Scope.Declaration ':+ scope)
    indexType = \case
      Type0.Declaration index -> types Vector.! index
