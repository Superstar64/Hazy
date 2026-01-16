module Stage2.Index.Term where

import Data.Void (absurd)
import Stage2.Scope (Declaration, Environment (..), Global, Pattern)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Prelude hiding (Functor, map)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Pattern :: !Bound -> Index (Pattern ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global

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
  map (after Shift.:. before) index = Shift.map after (Shift.map before index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) _ = absurd abort

data Category scope scope' where
  Lift :: Shift.Category scope scope' -> Category scope scope'
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  ReplaceWildcard :: Category (Pattern ':+ scope) (Declaration ':+ scope)
  SimplifyPattern :: Int -> Category (Pattern ':+ scope) (Pattern ':+ Pattern ':+ scope)
  RenamePattern :: (Int -> Int) -> Category (Pattern ':+ scope) (Pattern ':+ scope)
  SimplifyList :: Category (Pattern ':+ scope) (Pattern ':+ scope)
  LetPattern :: Category (Pattern ':+ scope) (Pattern ':+ Declaration ':+ scope)

general :: Category scope scope' -> Shift.Category scope scope'
general = \case
  Lift category -> category
  Over category -> Shift.Over (general category)
  ReplaceWildcard -> Shift.Unshift (error "bad unshift") Shift.:. Shift.Over Shift.Shift
  SimplifyPattern _ -> Shift.Shift
  RenamePattern _ -> Shift.Id
  SimplifyList -> Shift.Id
  LetPattern -> Shift.Over Shift.Shift

class (Shift.Functor term) => Functor term where
  map ::
    Category scope scope' ->
    term scope ->
    term scope'

instance Functor Index where
  map (Lift category) index = Shift.map category index
  map (Over category) (Shift index) = Shift $ map category index
  map (Over _) (Declaration index) = Declaration index
  map (Over _) (Pattern bound) = Pattern bound
  map ReplaceWildcard index = case index of
    Pattern At -> Declaration 0
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
    Pattern At -> Shift $ Declaration 0
    Pattern (Select index bound) ->
      Pattern (Select index bound)
    Shift index -> Shift (Shift index)

mapDefault :: (Functor term) => Shift.Category scope scope' -> term scope -> term scope'
mapDefault = map . Lift

data Bound
  = At
  | Select !Int !Bound
  deriving (Show, Read, Eq)
