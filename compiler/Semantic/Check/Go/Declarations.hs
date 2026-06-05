module Semantic.Check.Go.Declarations (Declarations (..), fromFunctor) where

import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import qualified Semantic.Check.Functor.Declarations as Functor (Declarations (..))
import Semantic.Check.Go.TypeDeclaration (TypeDeclaration)
import Semantic.Stage (Check)
import Semantic.Tree.Declaration (Declaration)
import Semantic.Tree.Declarations (Declarations (..))
import {-# SOURCE #-} Semantic.Tree.Instance (Instance)
import Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

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
