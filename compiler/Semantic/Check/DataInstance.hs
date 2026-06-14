module Semantic.Check.DataInstance where

import Control.Monad.ST (ST)
import Data.Foldable (toList, traverse_)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Order (orderListInt')
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as Constructor (info)
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.EntryInstance (EntryInstance (..))
import Semantic.Check.Temporary.SelectorInfo (Select (..), SelectorInfo (..))
import Semantic.Check.Temporary.UpdateInfo (UpdateInfo (..))
import qualified Semantic.Check.Temporary.UpdateInfo as UpdateInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Selector as Selector (Index (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Tree.Selector (Selector (..))
import qualified Semantic.Tree.Selector as Selector (Uniform (..))
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data DataInstance s scope = DataInstance
  { position :: !Position,
    types :: !(Strict.Vector (Unify.Type s scope)),
    constructors :: !(Strict.Vector (ConstructorInstance s scope)),
    selectors :: !(Strict.Vector Selector)
  }

baseType :: DataInstance s scope -> Type2.Index scope -> Unify.Type s scope
baseType DataInstance {types} typeIndex = foldl Unify.call (Unify.constructor typeIndex) types

constructorFunction :: DataInstance s scope -> Constructor.Index scope -> Unify.Type s scope
constructorFunction instancex@DataInstance {constructors} Constructor.Index {typeIndex, constructorIndex} =
  ConstructorInstance.function
    (constructors Strict.Vector.! constructorIndex)
    (baseType instancex typeIndex)

selectorFunction :: DataInstance s scope -> Selector.Index scope -> Unify.Type s scope
selectorFunction instancex (Selector.Index typeIndex selectorIndex) =
  baseType instancex typeIndex `Unify.function` selectorType instancex selectorIndex

selectorType :: DataInstance s scope -> Int -> Unify.Type s scope
selectorType DataInstance {constructors, selectors} selectorIndex = entry
  where
    Selector {first, index} = selectors Strict.Vector.! selectorIndex
    ConstructorInstance {entries} = constructors Strict.Vector.! first
    EntryInstance {entry} = entries Strict.Vector.! index

selectorInfo :: DataInstance s scope -> Int -> SelectorInfo s scope
selectorInfo DataInstance {position, constructors, selectors} index = case selectors Strict.Vector.! index of
  Selector {first, index, uniform = Selector.Uniform} -> Uniform {position, strict}
    where
      ConstructorInstance {entries} = constructors Strict.Vector.! first
      EntryInstance {strict} = entries Strict.Vector.! index
  Selector {uniform = Selector.Disjoint {indexes}} ->
    Disjoint
      { select = Strict.Vector.imap pick indexes
      }
    where
      pick index selectIndex =
        Select
          { selectIndex,
            constructorInfo = Constructor.info (constructors Strict.Vector.! index)
          }

updateInfo :: DataInstance s scope -> UpdateInfo s scope
updateInfo DataInstance {constructors, selectors} =
  UpdateInfo
    { updateInfo = Strict.Vector.imap update constructors
    }
  where
    update :: Int -> ConstructorInstance s scope -> UpdateInfo.Update s scope
    update constructorIndex constructorInstance@ConstructorInstance {entries} =
      UpdateInfo.Update
        { constructorInfo = ConstructorInstance.info constructorInstance,
          selectorIndexes =
            let selections = do
                  Selector {index, uniform} <- toList selectors
                  case uniform of
                    Selector.Uniform -> [index]
                    Selector.Disjoint {indexes} ->
                      case indexes Strict.Vector.! constructorIndex of
                        Strict.Nothing -> []
                        Strict.Just index -> [index]
                combine = \case
                  [index] -> Strict.Just index
                  [] -> Strict.Nothing
                  _ -> error "duplicate selector"
             in orderListInt' combine (length entries) (zip selections selections)
        }

mark :: Context s scope -> DataInstance s scope -> ST s ()
mark context DataInstance {constructors} =
  traverse_ (ConstructorInstance.mark context) constructors
