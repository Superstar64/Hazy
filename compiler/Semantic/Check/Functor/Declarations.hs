module Semantic.Check.Functor.Declarations where

import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Heptafoldable (Heptafoldable (heptafoldMap))
import Data.Heptafunctor (Heptafunctor (heptamap))
import Data.Heptatraversable (Heptatraversable (heptatraverse), heptafoldMapDefault, heptamapDefault)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Semantic.Check.Functor.Annotated (Annotated (..), NoLabel (..))
import Semantic.Check.Functor.Instance.Key (Key (..))
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Term as Label (TermBinding)
import qualified Semantic.Label.Binding.Type as Label (TypeBinding)
import Semantic.Layout (Group)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Declaration as Semantic (Declaration)
import qualified Semantic.Tree.Declaration as Semantic.Declaration
import qualified Semantic.Tree.Declarations as Semantic
  ( Declarations (..),
  )
import qualified Semantic.Tree.Instance as Semantic (Instance)
import qualified Semantic.Tree.TypeDeclaration as Semantic (TypeDeclaration)
import qualified Semantic.Tree.TypeDeclaration as Semantic.TypeDeclaration
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic (TypeDeclarationExtra)
import Syntax.Variable (Qualifiers)

data Declarations scope a b c d e f g = Declarations
  { terms :: !(Vector (Annotated Label.TermBinding a b)),
    types :: !(Vector (Annotated Label.TypeBinding c d)),
    typeExtras :: !(Vector e),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Annotated NoLabel f g))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Annotated NoLabel f g)))
  }

instance Heptafunctor (Declarations scope) where
  heptamap = heptamapDefault

instance Heptafoldable (Declarations scope) where
  heptafoldMap = heptafoldMapDefault

instance Heptatraversable (Declarations scope) where
  heptatraverse
    f
    g
    h
    i
    j
    k
    l
    Declarations
      { terms,
        types,
        typeExtras,
        dataInstances,
        classInstances
      } =
      Declarations
        <$> traverse (bitraverse f g) terms
        <*> traverse (bitraverse h i) types
        <*> traverse j typeExtras
        <*> traverse (traverse (bitraverse k l)) dataInstances
        <*> traverse (traverse (bitraverse k l)) classInstances

mapWithKey ::
  (Int -> a1 -> a2) ->
  (Int -> b1 -> b2) ->
  (Int -> c1 -> c2) ->
  (Int -> d1 -> d2) ->
  (Int -> e1 -> e2) ->
  (Key scope -> f1 -> f2) ->
  (Key scope -> g1 -> g2) ->
  Declarations scope a1 b1 c1 d1 e1 f1 g1 ->
  Declarations scope a2 b2 c2 d2 e2 f2 g2
mapWithKey
  f1
  f2
  f3
  f4
  f5
  f6
  f7
  Declarations
    { terms,
      types,
      typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Vector.imap (\i -> bimap (f1 i) (f2 i)) terms,
        types = Vector.imap (\i -> bimap (f3 i) (f4 i)) types,
        typeExtras = Vector.imap f5 typeExtras,
        dataInstances = Vector.imap datax dataInstances,
        classInstances = Vector.imap classx classInstances
      }
    where
      datax index = Map.mapWithKey $
        \classKey -> let key = Data {index, classKey} in bimap (f6 key) (f7 key)
      classx index = Map.mapWithKey $
        \dataKey -> let key = Class {index, dataKey} in bimap (f6 key) (f7 key)

fromStage2 ::
  Qualifiers ->
  Semantic.Declarations locality Group Resolve scope ->
  Declarations
    scope
    (Semantic.Declaration locality Group Resolve scope)
    (Semantic.Declaration locality Group Resolve scope)
    (Semantic.TypeDeclaration locality Group Resolve scope)
    (Semantic.TypeDeclaration locality Group Resolve scope)
    (Semantic.TypeDeclarationExtra Group Resolve scope)
    (Semantic.Instance Group Resolve scope)
    (Semantic.Instance Group Resolve scope)
fromStage2
  path
  Semantic.Declarations
    { terms,
      types,
      typeExtras,
      dataInstances,
      classInstances
    } =
    let termDeclaration term@meta@content =
          Annotated
            { label = Semantic.Declaration.labelBinding path term,
              meta,
              content
            }
        typeDeclaration typex@meta@content =
          Annotated
            { label = Semantic.TypeDeclaration.labelBinding path typex,
              meta,
              content
            }
        instancex meta@content =
          Annotated
            { label = NoLabel,
              meta,
              content
            }
     in Declarations
          { terms = termDeclaration <$> terms,
            types = typeDeclaration <$> types,
            typeExtras,
            dataInstances = fmap instancex <$> dataInstances,
            classInstances = fmap instancex <$> classInstances
          }
