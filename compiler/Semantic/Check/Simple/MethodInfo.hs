module Semantic.Check.Simple.MethodInfo where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Data.Kind (Type)
import Semantic.Scope (Environment)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift

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
