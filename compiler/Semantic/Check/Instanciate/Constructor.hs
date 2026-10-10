module Semantic.Check.Instanciate.Constructor where

import Control.Monad.ST (ST)
import Core.Instanciate (Instanciated, Store (Store))
import Core.Tree.Constructor (Constructor, ConstructorF (..))
import Core.Tree.Entry (entry)
import Core.Tree.Type ((-#>))
import Data.Foldable (traverse_)
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Instanciate.Entry as EntryInstance
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Tree.Brand as Brand

type ConstructorInstance s scope = ConstructorF Instanciated (Unify.Logical s scope) scope

instanciate ::
  Position ->
  Brand ->
  Strict.Vector (Unify.Type s scope) ->
  Constructor (Local ':+ scope) ->
  ConstructorInstance s scope
instanciate position brand fresh Constructor {name, syntax, entries} =
  Constructor
    { brand = Store brand,
      name,
      syntax,
      entries = EntryInstance.instanciate position fresh <$> entries
    }

info :: ConstructorInstance s scope -> ConstructorInfo s scope
info Constructor {entries, brand = Store brand} = case brand of
  Brand.Newtype -> Newtype
  Brand.Boxed -> ConstructorInfo {entries = EntryInstance.info <$> entries}

types :: ConstructorInstance s scope -> Strict.Vector (Unify.Type s scope)
types Constructor {entries} = entry <$> entries

function :: ConstructorInstance s scope -> Unify.Type s scope -> Unify.Type s scope
function Constructor {entries} base = foldr ((-#>) . entry) base entries

mark :: Context s scope -> ConstructorInstance s scope -> ST s ()
mark context Constructor {entries} =
  traverse_ (EntryInstance.mark context) entries
