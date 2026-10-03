module Semantic.Check.ConstructorInstance where

import Control.Monad.ST (ST)
import Core.Tree.Type ((-#>))
import Data.Foldable (traverse_)
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.EntryInstance (EntryInstance, entry)
import qualified Semantic.Check.EntryInstance as EntryInstance
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo (..))
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Unify as Unify
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Tree.Brand as Brand
import qualified Syntax.Variable as Variable

data ConstructorInstance s scope = ConstructorInstance
  { brand :: !Brand,
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (EntryInstance s scope))
  }

info :: ConstructorInstance s scope -> ConstructorInfo s scope
info ConstructorInstance {entries, brand} = case brand of
  Brand.Newtype -> Newtype
  Brand.Boxed -> ConstructorInfo {entries = EntryInstance.info <$> entries}

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types ConstructorInstance {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function ConstructorInstance {entries} base = foldr ((-#>) . entry) base entries

mark :: Context s scope -> ConstructorInstance s scope -> ST s ()
mark context ConstructorInstance {entries} =
  traverse_ (EntryInstance.mark context) entries
