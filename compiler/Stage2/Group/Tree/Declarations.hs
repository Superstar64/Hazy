module Stage2.Group.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Graph.StronglyConnected as StronglyConnected
import qualified Stage2.Group.Functor.Term.Declarations as Term
import qualified Stage2.Group.Functor.Type.Declarations as Type
import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Declaration (Declaration)
import qualified Stage2.Group.Tree.Declaration as Declartion
import Stage2.Group.Tree.Group (Group)
import qualified Stage2.Group.Tree.Group as Group
import Stage2.Group.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage2.Group.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Tree.Declarations as Proper (Declarations (..))
import Stage2.Tree.Instance (Instance)
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
  Term.Declarations (StronglyConnected.Component (Term0.Index scope)) ->
  Type.Declarations (StronglyConnected.Component (Type0.Index scope)) ->
  Proper.Declarations scope ->
  Declarations scope
group
  indexTerm
  indexType
  Term.Declarations {terms, shared}
  Type.Declarations {types}
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
