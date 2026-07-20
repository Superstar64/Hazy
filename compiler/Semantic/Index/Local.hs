module Semantic.Index.Local where

import Data.Kind (Type)
import Semantic.Scope (Environment (..), Local)

type Index :: Environment -> Type
data Index scopes where
  Local :: !Int -> Index (Local ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)

instance Eq (Index scope) where
  Local local1 == Local local2 = local1 == local2
  Shift index1 == Shift index2 = index1 == index2
  _ == _ = False

instance Show (Index scope) where
  showsPrec d = \case
    Local local -> showParen (d > 10) $ showString "Local " . showsPrec 11 local
    Shift index -> showParen (d > 10) $ showString "Shift " . showsPrec 11 index
