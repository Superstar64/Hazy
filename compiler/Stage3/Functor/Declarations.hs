module Stage3.Functor.Declarations where

import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Octafoldable (Octafoldable (octafoldMap))
import Data.Octafunctor (Octafunctor (octamap))
import Data.Octatraversable (Octatraversable (octatraverse), octafoldMapDefault, octamapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage1.Variable (Qualifiers)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Term as Label (TermBinding)
import qualified Stage2.Label.Binding.Type as Label (TypeBinding)
import qualified Stage2.Tree.Declarations as Stage2
  ( Declarations (..),
  )
import qualified Stage2.Tree.Instance as Stage2 (Instance)
import qualified Stage2.Tree.Shared as Stage2 (Shared (..))
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration)
import qualified Stage2.Tree.TermDeclaration as Stage2.TermDeclaration
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage3.Functor.Annotated (Annotated (..), NoLabel (..))
import Stage3.Functor.Instance.Key (Key (..))

data Declarations scope a b c d e f g h = Declarations
  { terms :: !(Vector (Annotated Label.TermBinding a b)),
    types :: !(Vector (Annotated Label.TypeBinding d e)),
    shared :: !(Vector c),
    typeExtras :: !(Vector f),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Annotated NoLabel g h))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Annotated NoLabel g h)))
  }

instance Octafunctor (Declarations scope) where
  octamap = octamapDefault

instance Octafoldable (Declarations scope) where
  octafoldMap = octafoldMapDefault

instance Octatraversable (Declarations scope) where
  octatraverse
    f
    g
    h
    i
    j
    k
    l
    m
    Declarations
      { terms,
        types,
        shared,
        typeExtras,
        dataInstances,
        classInstances
      } =
      Declarations
        <$> traverse (bitraverse f g) terms
        <*> traverse (bitraverse i j) types
        <*> traverse h shared
        <*> traverse k typeExtras
        <*> traverse (traverse (bitraverse l m)) dataInstances
        <*> traverse (traverse (bitraverse l m)) classInstances

mapWithKey ::
  (Int -> a1 -> a2) ->
  (Int -> b1 -> b2) ->
  (Int -> c1 -> c2) ->
  (Int -> d1 -> d2) ->
  (Int -> e1 -> e2) ->
  (Int -> f1 -> f2) ->
  (Key scope -> g1 -> g2) ->
  (Key scope -> h1 -> h2) ->
  Declarations scope a1 b1 c1 d1 e1 f1 g1 h1 ->
  Declarations scope a2 b2 c2 d2 e2 f2 g2 h2
mapWithKey
  f1
  f2
  f3
  f4
  f5
  f6
  f7
  f8
  Declarations
    { terms,
      types,
      shared,
      typeExtras,
      dataInstances,
      classInstances
    } =
    Declarations
      { terms = Vector.imap (\i -> bimap (f1 i) (f2 i)) terms,
        types = Vector.imap (\i -> bimap (f4 i) (f5 i)) types,
        shared = Vector.imap f3 shared,
        typeExtras = Vector.imap f6 typeExtras,
        dataInstances = Vector.imap datax dataInstances,
        classInstances = Vector.imap classx classInstances
      }
    where
      datax index = Map.mapWithKey $
        \classKey -> let key = Data {index, classKey} in bimap (f7 key) (f8 key)
      classx index = Map.mapWithKey $
        \dataKey -> let key = Class {index, dataKey} in bimap (f7 key) (f8 key)

fromStage2 ::
  Qualifiers ->
  Stage2.Declarations scope ->
  Declarations
    scope
    (Stage2.TermDeclaration scope)
    (Stage2.TermDeclaration scope)
    (Stage2.Shared scope)
    (Stage2.TypeDeclaration scope)
    (Stage2.TypeDeclaration scope)
    (Stage2.TypeDeclaration scope)
    (Stage2.Instance scope)
    (Stage2.Instance scope)
fromStage2
  path
  Stage2.Declarations
    { terms,
      types,
      shared,
      dataInstances,
      classInstances
    } =
    let termDeclaration term@meta@content =
          Annotated
            { label = Stage2.TermDeclaration.labelBinding path term,
              meta,
              content
            }
        typeDeclaration typex@meta@content =
          Annotated
            { label = Stage2.TypeDeclaration.labelBinding path typex,
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
            shared,
            typeExtras = types,
            dataInstances = fmap instancex <$> dataInstances,
            classInstances = fmap instancex <$> classInstances
          }
