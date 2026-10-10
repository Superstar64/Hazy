module Semantic.Check.Info.Update where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

newtype UpdateInfo scope = UpdateInfo
  { updateInfo :: Strict.Vector (Update scope)
  }
  deriving (Show)

instance Scope.Show UpdateInfo where
  showsPrec = showsPrec

instance Shift0.Functor UpdateInfo where
  map = Shift.mapDefault

instance Shift.Functor UpdateInfo where
  map category UpdateInfo {updateInfo} =
    UpdateInfo
      { updateInfo = Shift.map category <$> updateInfo
      }

data Update scope = Update
  { constructorInfo :: !(ConstructorInfo scope),
    selectorIndexes :: !(Strict.Vector (Strict.Maybe Int))
  }
  deriving (Show)

instance Shift0.Functor Update where
  map = Shift.mapDefault

instance Shift.Functor Update where
  map category Update {constructorInfo, selectorIndexes} =
    Update
      { constructorInfo = Shift.map category constructorInfo,
        selectorIndexes
      }
