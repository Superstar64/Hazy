module Stage2.Group.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import Stage2.Group.Tree.Group (Group)
import Stage2.Group.Tree.TermDeclaration (TermDeclaration)
import Stage2.Group.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Tree.Instance (Instance)
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

data Declarations scope = Declarations
  { terms :: !(Vector (TermDeclaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    shared :: !(Vector (Group scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)
