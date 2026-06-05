module Semantic.Index.Term where

import qualified Data.Strict.Maybe as Strict
import Data.Void (absurd)
import {-# SOURCE #-} qualified Semantic.Index.Term0 as Term0
import Semantic.Scope (Declaration, Environment (..), Global, GroupTerm, Pattern)
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Prelude hiding (Functor, map)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Pattern :: !Bound -> Index (Pattern ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global
  Group :: !Int -> Index (GroupTerm ':+ scope)

instance Eq (Index scope) where
  Declaration local1 == Declaration local2 = local1 == local2
  Pattern bound1 == Pattern bound2 = bound1 == bound2
  Shift index1 == Shift index2 = index1 == index2
  Global global1 local1 == Global global2 local2 = global1 == global2 && local1 == local2
  _ == _ = False

instance Show (Index scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Pattern bound -> showParen (d > 10) $ showString "Pattern " . showsPrec 11 bound
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
  map (Shift.Over category) (Shift index) = Shift $ Shift.map category index
  map (Shift.Over _) (Declaration index) = Declaration index
  map (Shift.Over _) (Pattern bound) = Pattern bound
  map (Shift.Over _) (Group index) = Group index
  map (after Shift.:. before) index = Shift.map after (Shift.map before index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) _ = absurd abort
  map (Shift.GroupTerm term) (Declaration index)
    | Strict.Just index <- term (Term0.Declaration index) = Group index
  map (Shift.GroupTerm term) (Global global local)
    | Strict.Just index <- term (Term0.Global global local) = Group index
  map Shift.GroupTerm {} index = Shift index
  map Shift.GroupType {} index = Shift index
  map (Shift.UngroupTerm term) (Group index) = term index
  map Shift.UngroupTerm {} (Shift index) = index
  map Shift.UngroupType {} (Shift index) = index

data Bound
  = At
  | Select !Int !Bound
  deriving (Show, Read, Eq)
