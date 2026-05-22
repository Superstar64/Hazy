module Stage2.Index.Type where

import qualified Data.Strict.Maybe as Strict
import Data.Void (absurd, vacuous)
import {-# SOURCE #-} qualified Stage2.Index.Type0 as Type0
import Stage2.Scope (Declaration, Environment (..), Global, Group, Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global
  Group :: !Int -> Index (Group ':+ scopes)

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

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map Shift.Id index = index
  map Shift.Shift index = Shift index
  map (Shift.Over category) (Shift index) = Shift (Shift.map category index)
  map (Shift.Over _) (Declaration index) = Declaration index
  map (Shift.Over _) (Group index) = Group index
  map (after Shift.:. before) index = Shift.map after (Shift.map before index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) _ = absurd abort
  map (Shift.GroupType typex) (Declaration index)
    | Strict.Just index <- typex (Type0.Declaration index) = Group index
  map (Shift.GroupType typex) (Global global local)
    | Strict.Just index <- typex (Type0.Global global local) = Group index
  map Shift.GroupTerm {} index = Shift index
  map Shift.GroupType {} index = Shift index

unlocal :: Index (Local ':+ scope) -> Index scope
unlocal (Shift index) = index

instance Shift.PartialUnshift Index where
  partialUnshift _ (Shift index) = pure index
  partialUnshift abort _ = vacuous abort
