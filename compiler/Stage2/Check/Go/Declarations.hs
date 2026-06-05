module Stage2.Check.Go.Declarations (Declarations (..), fromFunctor) where

import Stage2.Stage (Check)
import Stage2.Tree.Declaration (Declaration)
import Stage2.Tree.Declarations (Declarations (..))
import {-# SOURCE #-} Stage2.Tree.Instance (Instance)
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage2.Check.Functor.Annotated as Functor (Annotated (..))
import qualified Stage2.Check.Functor.Declarations as Functor (Declarations (..))
import Stage2.Check.Go.TypeDeclaration (TypeDeclaration)

fromFunctor ::
  Functor.Declarations
    scope
    a
    (Declaration locality layout Check scope)
    b
    (TypeDeclaration locality layout Check scope)
    (TypeDeclarationExtra layout Check scope)
    d
    (Instance layout Check scope) ->
  Declarations locality layout Check scope
fromFunctor (Functor.Declarations {terms, types, typeExtras, dataInstances, classInstances}) =
  Declarations
    { terms = Functor.content <$> terms,
      types = Functor.content <$> types,
      typeExtras,
      dataInstances = fmap (fmap Functor.content) dataInstances,
      classInstances = fmap (fmap Functor.content) classInstances
    }
