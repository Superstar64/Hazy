module Semantic.Check.Go.Declarations (Declarations (..), fromFunctor) where

import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import qualified Semantic.Check.Functor.Declarations as Functor (Declarations (..))
import Semantic.Check.Go.TypeDeclaration (TypeDeclaration)
import Semantic.Tree.Declaration (Declaration)
import Semantic.Tree.Declarations (Declarations (..))
import {-# SOURCE #-} Semantic.Tree.Instance (Instance)
import Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

fromFunctor ::
  Functor.Declarations
    scope
    a1
    (Declaration solve logical locality loeb layout stage scope)
    a2
    (TypeDeclaration locality loeb layout stage scope)
    (TypeDeclarationExtra layout stage scope)
    a3
    (Instance solve layout stage scope) ->
  Declarations solve logical locality loeb layout stage scope
fromFunctor (Functor.Declarations {terms, types, typeExtras, dataInstances, classInstances}) =
  Declarations
    { terms = Functor.content <$> terms,
      types = Functor.content <$> types,
      typeExtras,
      dataInstances = fmap (fmap Functor.content) dataInstances,
      classInstances = fmap (fmap Functor.content) classInstances
    }
