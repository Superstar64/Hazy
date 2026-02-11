module Stage3.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import {-# SOURCE #-} Stage3.Tree.Instance (Instance)
import Stage3.Tree.TermDeclaration (LazyTermDeclaration)
import Stage3.Tree.TypeDeclaration (LazyTypeDeclaration)
import Stage3.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

data Declarations scope = Declarations
  { terms :: !(Vector (LazyTermDeclaration scope)),
    types :: !(Vector (LazyTypeDeclaration scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

fromFunctor ::
  Functor.Declarations
    scope
    a
    (LazyTermDeclaration scope)
    b
    (LazyTypeDeclaration scope)
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
