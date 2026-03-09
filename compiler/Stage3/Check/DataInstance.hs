module Stage3.Check.DataInstance where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import Stage2.Tree.Selector (Selector)
import Stage3.Check.ConstructorInstance (ConstructorInstance)
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
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
