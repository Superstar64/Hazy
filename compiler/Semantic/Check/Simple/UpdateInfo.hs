module Semantic.Check.Simple.UpdateInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Simple.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift

newtype UpdateInfo scope = UpdateInfo
  { updateInfo :: Strict.Vector (Update scope)
  }
  deriving (Show)

instance Scope.Show UpdateInfo where
  showsPrec = showsPrec

instance Shift UpdateInfo where
  shift = shiftDefault

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

instance Shift Update where
  shift = shiftDefault

instance Shift.Functor Update where
  map category Update {constructorInfo, selectorIndexes} =
    Update
      { constructorInfo = Shift.map category constructorInfo,
        selectorIndexes
      }
