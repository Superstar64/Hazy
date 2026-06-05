module Stage2.Check.Simple.MethodInfo where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute

type MethodInfo :: Environment -> Type
newtype MethodInfo scope = MethodInfo
  { constraintCount :: Int
  }
  deriving (Show)

instance Scope.Show MethodInfo where
  showsPrec = showsPrec

instance Shift MethodInfo where
  shift = shiftDefault

instance Shift.Functor MethodInfo where
  map = Shift2.mapDefault

instance Shift2.Functor MethodInfo where
  map = Substitute.mapDefault

instance Substitute.Functor MethodInfo where
  map _ MethodInfo {constraintCount} =
    MethodInfo
      { constraintCount
      }
