module Stage3.Check.DataInstance where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor (Index (..))
import qualified Stage2.Index.Selector as Selector (Index (..))
import Stage2.Tree.Selector (Selector (..))
import qualified Stage2.Tree.Selector as Selector (Uniform (..))
import Stage3.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Stage3.Check.ConstructorInstance as Constructor (info)
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.EntryInstance (EntryInstance (..))
import Stage3.Tree.SelectorInfo (Select (..), SelectorInfo (..))
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data DataInstance s scope = DataInstance
  { types :: !(Strict.Vector (Unify.Type s scope)),
    constructors :: !(Strict.Vector (ConstructorInstance s scope)),
    selectors :: !(Strict.Vector Selector)
  }

constructorFunction :: DataInstance s scope -> Constructor.Index scope -> Unify.Type s scope
constructorFunction DataInstance {types, constructors} Constructor.Index {typeIndex, constructorIndex} =
  ConstructorInstance.function (constructors Strict.Vector.! constructorIndex) base
  where
    root = Unify.constructor typeIndex
    base = foldl Unify.call root types

selectorFunction :: DataInstance s scope -> Selector.Index scope -> Unify.Type s scope
selectorFunction DataInstance {types, constructors, selectors} (Selector.Index typeIndex selectorIndex) =
  base `Unify.function` entry
  where
    root = Unify.constructor typeIndex
    base = foldl Unify.call root types
    Selector {first, index} = selectors Strict.Vector.! selectorIndex
    ConstructorInstance {entries} = constructors Strict.Vector.! first
    EntryInstance {entry} = entries Strict.Vector.! index

selectorInfo :: DataInstance s scope -> Int -> SelectorInfo
selectorInfo DataInstance {constructors, selectors} index = case selectors Strict.Vector.! index of
  Selector {uniform = Selector.Uniform {strict}} ->
    Uniform
      { strict
      }
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
