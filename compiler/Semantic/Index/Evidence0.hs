module Semantic.Index.Evidence0 where

import Data.Kind (Type)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope

type Index :: Environment -> Type
data Index scope where
  Assumed :: !Int -> Index (Scope.Local ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)

instance Eq (Index scope) where
  Assumed left == Assumed right = left == right
  Shift left == Shift right = left == right
  _ == _ = False

instance Show (Index scopes) where
  showsPrec d (Assumed i) =
    showParen (d > 10) $
      showString "Assumed "
        . showsPrec 11 i
  showsPrec d (Shift ix) =
    showParen (d > 10) $
      showString "Shift "
        . showsPrec 11 ix
