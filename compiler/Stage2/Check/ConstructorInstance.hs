module Stage2.Check.ConstructorInstance where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.Vector.Strict as Strict
import Stage1.Tree.Brand (Brand)
import qualified Stage1.Tree.Brand as Brand
import Stage2.Check.Context (Context)
import Stage2.Check.EntryInstance (EntryInstance, entry)
import qualified Stage2.Check.EntryInstance as EntryInstance
import Stage2.Check.Temporary.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} qualified Stage2.Unify as Unify

data ConstructorInstance s scope = ConstructorInstance
  { entries :: !(Strict.Vector (EntryInstance s scope)),
    brand :: !Brand
  }

info :: ConstructorInstance s scope -> ConstructorInfo s scope
info ConstructorInstance {entries, brand} = case brand of
  Brand.Newtype -> Newtype
  Brand.Boxed -> ConstructorInfo {entries = EntryInstance.info <$> entries}

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types ConstructorInstance {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function ConstructorInstance {entries} base = foldr (Unify.function . entry) base entries

mark :: Context s scope -> ConstructorInstance s scope -> ST s ()
mark context ConstructorInstance {entries} =
  traverse_ (EntryInstance.mark context) entries
