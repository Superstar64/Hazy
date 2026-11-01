module Semantic.Tree.Info.Update where

import qualified Core.Tree.Type as Core
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data UpdateInfo scope
  = Newtype
  | Updates
      { updates :: Strict.Vector (Update scope)
      }
  deriving (Show)

instance Scope.Show UpdateInfo where
  showsPrec = showsPrec

instance Shift0.Functor UpdateInfo where
  map = Shift.mapDefault

instance Shift.Functor UpdateInfo where
  map category = \case
    Newtype -> Newtype
    Updates {updates} ->
      Updates
        { updates = Shift.map category <$> updates
        }

data Update scope = Update
  { levities :: !(Strict.Vector (Core.Type scope)),
    selectorIndexes :: !(Strict.Vector (Strict.Maybe Int))
  }
  deriving (Show)

instance Shift0.Functor Update where
  map = Shift.mapDefault

instance Shift.Functor Update where
  map category Update {levities, selectorIndexes} =
    Update
      { levities = Shift.map category <$> levities,
        selectorIndexes
      }
