module Stage3.Check.ConstructorInstance where

import qualified Data.Vector.Strict as Strict
import Stage3.Check.EntryInstance (EntryInstance, entry)
import Stage3.Tree.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} qualified Stage3.Unify as Unify

newtype ConstructorInstance s scope = ConstructorInstance
  { entries :: Strict.Vector (EntryInstance s scope)
  }

info :: ConstructorInstance s scope -> ConstructorInfo
info ConstructorInstance {entries} =
  ConstructorInfo
    { parameterCount = length entries
    }

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types ConstructorInstance {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function ConstructorInstance {entries} base = foldr (Unify.function . entry) base entries
