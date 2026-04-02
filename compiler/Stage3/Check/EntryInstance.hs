module Stage3.Check.EntryInstance where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage2.Scope (Environment (..), Local)
import Stage3.Check.Context (Context)
import qualified Stage3.Check.Mask as Mask
import qualified Stage3.Simple.Type as Type
import Stage3.Temporary.EntryInfo (EntryInfo (..))
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Entry (Entry (..))

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
