module Semantic.Check.EntryInstance where

import Control.Monad.ST (ST)
import Core.Tree.Entry (Entry (..))
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Type as Type
import Semantic.Check.Temporary.EntryInfo (EntryInfo (..))
import Semantic.Scope (Environment (..), Local)
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data EntryInstance s scope = EntryInstance
  { position :: !Position,
    entry :: !(Unify.Type s scope),
    strict :: !(Unify.Type s scope)
  }

instanciate :: Position -> Strict.Vector (Unify.Type s scope) -> Entry (Local ':+ scope) -> EntryInstance s scope
instanciate position fresh Entry {entry, strict} =
  EntryInstance
    { position,
      entry = Type.instanciate fresh entry,
      strict = Type.instanciate fresh strict
    }

info :: EntryInstance s scope -> EntryInfo s scope
info EntryInstance {position, strict} = EntryInfo {position, strict}

mark :: Context s scope -> EntryInstance s scope -> ST s ()
mark context EntryInstance {position, strict} =
  Unify.mark context position Mask.Known strict
