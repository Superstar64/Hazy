module Semantic.Check.EntryInstance where

import Control.Monad.ST (ST)
import Core.Instanciate (Instanciated, Store (Store))
import Core.Substitute (substituteType)
import Core.Tree.Entry (Entry, EntryF (..))
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import Semantic.Check.Temporary.EntryInfo (EntryInfo (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type EntryInstance s scope = EntryF Instanciated (Unify.Logical s scope) scope

instanciate :: Position -> Strict.Vector (Unify.Type s scope) -> Entry (Local ':+ scope) -> EntryInstance s scope
instanciate position fresh Entry {entry, strict} =
  Entry
    { position = Store position,
      entry = substituteType (Strict.Vector.toLazy fresh) entry,
      strict = substituteType (Strict.Vector.toLazy fresh) strict
    }

info :: EntryInstance s scope -> EntryInfo s scope
info Entry {position = Store position, strict} = EntryInfo {position, strict}

mark :: Context s scope -> EntryInstance s scope -> ST s ()
mark context Entry {position = Store position, strict} =
  Unify.mark context position Mask.Known strict
