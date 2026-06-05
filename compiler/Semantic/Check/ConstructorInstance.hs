module Semantic.Check.ConstructorInstance where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Context (Context)
import Semantic.Check.EntryInstance (EntryInstance, entry)
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Tree.Brand as Brand

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
