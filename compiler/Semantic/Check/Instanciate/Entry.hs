module Semantic.Check.Instanciate.Entry where

import Control.Monad.ST (ST)
import Core.Instanciate (Instanciated, Store (Store))
import Core.Tree.Entry (Entry, EntryF (..))
import qualified Core.Type.Substitute as Type.Substitute
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Check.Temporary.EntryInfo (EntryInfo (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type EntryInstance s scope = EntryF Instanciated (Unify.Logical s scope) scope

instanciate :: Position -> Strict.Vector (Unify.Type s scope) -> Entry (Local ':+ scope) -> EntryInstance s scope
instanciate position fresh Entry {entry, levity} =
  Entry
    { position = Store position,
      entry = Type.Substitute.substitute fresh entry,
      levity = Type.Substitute.substitute fresh levity
    }

info :: EntryInstance s scope -> EntryInfo s scope
info Entry {position = Store position, levity} = EntryInfo {position, levity}

mark :: Context s scope -> EntryInstance s scope -> ST s ()
mark context Entry {position = Store position, levity} =
  Unify.mark context position Mask.Known levity
