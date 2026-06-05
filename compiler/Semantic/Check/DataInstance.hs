module Semantic.Check.DataInstance where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as Constructor (info)
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.EntryInstance (EntryInstance (..))
import Semantic.Check.Temporary.SelectorInfo (Select (..), SelectorInfo (..))
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Selector as Selector (Index (..))
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

mark :: Context s scope -> DataInstance s scope -> ST s ()
mark context DataInstance {constructors} =
  traverse_ (ConstructorInstance.mark context) constructors
