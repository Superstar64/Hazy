module Stage3.Check.EntryInstance where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment (..), Local)
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Entry (Entry (..))

data EntryInstance s scope = EntryInstance
  { entry :: !(Unify.Type s scope),
    strict :: !Bool
  }

instanciate :: Strict.Vector (Unify.Type s scope) -> Entry (Local ':+ scope) -> EntryInstance s scope
instanciate fresh Entry {entry, strict} =
  EntryInstance
    { entry = Type.instanciate fresh entry,
      strict
    }
