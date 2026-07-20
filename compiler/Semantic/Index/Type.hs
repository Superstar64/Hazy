module Semantic.Index.Type where

import Semantic.Scope (Declaration, Environment (..), Global, GroupType, Local)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global
  Group :: !Int -> Index (GroupType ':+ scopes)

instance Eq (Index scope) where
  Declaration local1 == Declaration local2 = local1 == local2
  Shift index1 == Shift index2 = index1 == index2
  Global global1 local1 == Global global2 local2 = global1 == global2 && local1 == local2
  _ == _ = False

instance Ord (Index scope) where
  Declaration index1 `compare` Declaration index2 = index1 `compare` index2
  Declaration {} `compare` Shift {} = LT
  Shift {} `compare` Declaration {} = GT
  Shift index1 `compare` Shift index2 = index1 `compare` index2
  Shift {} `compare` Group {} = LT
  Global global1 local1 `compare` Global global2 local2 = (global1, local1) `compare` (global2, local2)
  Group {} `compare` Shift {} = GT
  Group index1 `compare` Group index2 = index1 `compare` index2

instance Show (Index scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Shift index -> showParen (d > 10) $ showString "Shift " . showsPrec 11 index
    Group index -> showParen (d > 10) $ showString "Group " . showsPrec 11 index
    Global global local ->
      showParen (d > 10) $
        showString "Global "
          . showsPrec 11 global
          . showString " "
          . showsPrec 11 local

unlocal :: Index (Local ':+ scope) -> Index scope
unlocal (Shift index) = index
