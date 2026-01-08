module Stage3.Functor.Declarations where

import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Hexafoldable (Hexafoldable (hexafoldMap))
import Data.Hexafunctor (Hexafunctor (hexamap))
import Data.Hexatraversable (Hexatraversable (hexatraverse), hexafoldMapDefault, hexamapDefault)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage1.Variable (Qualifiers)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Term as Label (TermBinding)
import qualified Stage2.Label.Binding.Type as Label (TypeBinding)
import qualified Stage2.Tree.Declarations as Stage2
  ( Declarations
      ( Declarations,
        classInstances,
        dataInstances,
        terms,
        types
      ),
  )
import qualified Stage2.Tree.Instance as Stage2 (Instance)
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration)
import qualified Stage2.Tree.TermDeclaration as Stage2.TermDeclaration
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage3.Functor.Annotated (Annotated (..), NoLabel (..))
import Stage3.Functor.Instance.Key (Key (..))

data Declarations scope a b c d e f = Declarations
  { terms :: !(Vector (Annotated Label.TermBinding a b)),
    types :: !(Vector (Annotated Label.TypeBinding c d)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Annotated NoLabel e f))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Annotated NoLabel e f)))
  }

instance Hexafunctor (Declarations scope) where
  hexamap = hexamapDefault

instance Hexafoldable (Declarations scope) where
  hexafoldMap = hexafoldMapDefault

instance Hexatraversable (Declarations scope) where
  hexatraverse f g h i j k Declarations {terms, types, dataInstances, classInstances} =
    Declarations
      <$> traverse (bitraverse f g) terms
      <*> traverse (bitraverse h i) types
      <*> traverse (traverse (bitraverse j k)) dataInstances
      <*> traverse (traverse (bitraverse j k)) classInstances

mapWithKey ::
  (Int -> a1 -> a2) ->
  (Int -> b1 -> b2) ->
  (Int -> c1 -> c2) ->
  (Int -> d1 -> d2) ->
  (Key scope -> e1 -> e2) ->
  (Key scope -> f1 -> f2) ->
  Declarations scope a1 b1 c1 d1 e1 f1 ->
  Declarations scope a2 b2 c2 d2 e2 f2
mapWithKey f1 f2 f3 f4 f5 f6 Declarations {terms, types, dataInstances, classInstances} =
  Declarations
    { terms = Vector.imap (\i -> bimap (f1 i) (f2 i)) terms,
      types = Vector.imap (\i -> bimap (f3 i) (f4 i)) types,
      dataInstances = Vector.imap datax dataInstances,
      classInstances = Vector.imap classx classInstances
    }
  where
    datax index = Map.mapWithKey $
      \classKey -> let key = Data {index, classKey} in bimap (f5 key) (f6 key)
    classx index = Map.mapWithKey $
      \dataKey -> let key = Class {index, dataKey} in bimap (f5 key) (f6 key)

fromStage2 ::
  Qualifiers ->
  Stage2.Declarations scope ->
  Declarations
    scope
    (Stage2.TermDeclaration scope)
    (Stage2.TermDeclaration scope)
    (Stage2.TypeDeclaration scope)
    (Stage2.TypeDeclaration scope)
    (Stage2.Instance scope)
    (Stage2.Instance scope)
fromStage2
  path
  Stage2.Declarations
    { terms,
      types,
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
            dataInstances = fmap instancex <$> dataInstances,
            classInstances = fmap instancex <$> classInstances
          }
