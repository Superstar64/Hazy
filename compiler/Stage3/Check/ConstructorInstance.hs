module Stage3.Check.ConstructorInstance where

import qualified Data.Vector.Strict as Strict
import Stage1.Tree.Brand (Brand)
import qualified Stage1.Tree.Brand as Brand
import Stage3.Check.EntryInstance (EntryInstance, entry)
import qualified Stage3.Check.EntryInstance as EntryInstance
import Stage3.Tree.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data ConstructorInstance s scope = ConstructorInstance
  { entries :: !(Strict.Vector (EntryInstance s scope)),
    brand :: !Brand
  }

info :: ConstructorInstance s scope -> ConstructorInfo
info ConstructorInstance {entries, brand} = case brand of
  Brand.Newtype -> Newtype
  Brand.Boxed -> ConstructorInfo {entries = EntryInstance.info <$> entries}

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types ConstructorInstance {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function ConstructorInstance {entries} base = foldr (Unify.function . entry) base entries
