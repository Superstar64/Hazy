module Stage3.Tree.Declarations where

import Data.Kind (Type)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Stage (Check)
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import Stage3.Tree.Declaration (Declaration)
import {-# SOURCE #-} Stage3.Tree.Instance (Instance)
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import Stage3.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

type Declarations :: Locality -> Layout -> Environment -> Type
data Declarations locality layout scope = Declarations
  { terms :: !(Vector (Declaration locality layout scope)),
    types :: !(Vector (TypeDeclaration locality layout Check scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

fromFunctor ::
  Functor.Declarations
    scope
    a
    (Declaration locality layout scope)
    b
    (TypeDeclaration locality layout Check scope)
    (TypeDeclarationExtra scope)
    d
    (Instance scope) ->
  Declarations locality layout scope
fromFunctor (Functor.Declarations {terms, types, typeExtras, dataInstances, classInstances}) =
  Declarations
    { terms = Functor.content <$> terms,
      types = Functor.content <$> types,
      typeExtras,
      dataInstances = fmap (fmap Functor.content) dataInstances,
      classInstances = fmap (fmap Functor.content) classInstances
    }
