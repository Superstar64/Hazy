module Stage3.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import {-# SOURCE #-} Stage3.Tree.Instance (Instance)
import Stage3.Tree.TermDeclaration (TermDeclaration)
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import Stage3.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

data Declarations scope = Declarations
  { terms :: !(Vector (TermDeclaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

fromFunctor ::
  Functor.Declarations
    scope
    a
    (TermDeclaration scope)
    b
    (TypeDeclaration scope)
    (TypeDeclarationExtra scope)
    e
    (Instance scope) ->
  Declarations scope
fromFunctor (Functor.Declarations {terms, types, typeExtras, dataInstances, classInstances}) =
  Declarations
    { terms = Functor.content <$> terms,
      types = Functor.content <$> types,
      typeExtras,
      dataInstances = fmap (fmap Functor.content) dataInstances,
      classInstances = fmap (fmap Functor.content) classInstances
    }
