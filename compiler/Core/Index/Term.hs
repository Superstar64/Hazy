module Core.Index.Term
  ( Index (..),
    from,
    Bound (..),
  )
where

import Core.Shift (Category (..), Functor (..))
import Data.Void (absurd)
import Semantic.Index.Term (Bound (..))
import qualified Semantic.Index.Term as Semantic
import Semantic.Scope (Declaration, Environment (..), Global, Pattern, SimpleDeclaration, SimplePattern)
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Prelude hiding (Functor, map)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Pattern :: !Bound -> Index (Pattern ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global
  SimplePattern :: !Int -> Index (SimplePattern ':+ scopes)
  SimpleDeclaration :: Index (SimpleDeclaration ':+ scopes)

instance Show (Index scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Pattern bound -> showParen (d > 10) $ showString "Pattern " . showsPrec 11 bound
    Shift index -> showParen (d > 10) $ showString "Shift " . showsPrec 11 index
    Global global local ->
      showParen (d > 10) $
        showString "Global "
          . showsPrec 11 global
          . showString " "
          . showsPrec 11 local
    SimplePattern local -> showParen (d > 10) $ showString "SimplePattern " . showsPrec 11 local
    SimpleDeclaration -> showString "SimpleDeclaration"

from :: Semantic.Index scope -> Index scope
from = \case
  Semantic.Declaration index -> Declaration index
  Semantic.Pattern patternx -> Pattern patternx
  Semantic.Shift index -> Shift (from index)
  Semantic.Global global local -> Global global local
  Semantic.Group {} -> error "no group in stage4"

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map Shift.Id index = index
  map Shift.Shift index = Shift index
  map (Shift.Over category) (Shift index) = Shift $ Shift.map category index
  map (Shift.Over _) (Declaration index) = Declaration index
  map (Shift.Over _) (Pattern bound) = Pattern bound
  map (Shift.Over _) (SimplePattern index) = SimplePattern index
  map (Shift.Over _) SimpleDeclaration = SimpleDeclaration
  map (after Shift.:. before) index = Shift.map after (Shift.map before index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) _ = absurd abort
  map Shift.GroupTerm {} index = Shift index
  map Shift.GroupType {} index = Shift index
  map (Shift.UngroupTerm {}) (Shift index) = index
  map (Shift.UngroupType {}) (Shift index) = index

instance Functor Index where
  map (Lift category) index = Shift.map category index
  map (Over category) (Shift index) = Shift $ map category index
  map (Over _) (Declaration index) = Declaration index
  map (Over _) (Pattern bound) = Pattern bound
  map (Over _) (SimplePattern index) = SimplePattern index
  map (Over _) SimpleDeclaration = SimpleDeclaration
  map ReplaceWildcard index = case index of
    Pattern At -> SimpleDeclaration
    Pattern _ -> error "bad wildcard bind"
    Shift index -> Shift index
  map (SimplifyPattern target) index = case index of
    Pattern (Select source bound)
      | source == target -> Pattern bound
    Pattern _ -> shift index
    Shift _ -> shift index
  map (RenamePattern rename) index = case index of
    Pattern At -> Pattern At
    Pattern (Select index' bound) ->
      Pattern (Select (rename index') bound)
    Shift index -> Shift index
  map SimplifyList index = case index of
    Pattern At -> Pattern At
    Pattern (Select 0 bound) -> Pattern (Select 0 bound)
    Pattern (Select index bound) ->
      Pattern (Select 1 (Select (index - 1) bound))
    Shift index -> Shift index
  map LetPattern index = case index of
    Pattern At -> Shift SimpleDeclaration
    Pattern (Select index bound) ->
      Pattern (Select index bound)
    Shift index -> Shift (Shift index)
  map FinishPattern index = case index of
    Pattern (Select index At) -> SimplePattern index
    Pattern _ -> error "bad finish pattern"
    Shift index -> Shift index
  map FinishNewtype index = case index of
    SimplePattern n
      | n == 0 -> SimpleDeclaration
      | otherwise -> error "bad finish newtype"
    Shift index -> Shift index
  map (ReplaceIrrefutable target) index = case index of
    SimplePattern index
      | index == target -> Shift SimpleDeclaration
      | otherwise -> SimplePattern index
    Shift index -> Shift (Shift index)
