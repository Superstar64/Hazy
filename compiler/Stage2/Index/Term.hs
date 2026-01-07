module Stage2.Index.Term where

import Data.Void (absurd)
import Stage2.Scope (Declaration, Environment (..), Global, Pattern)
import Stage2.Shift (Shift, shiftDefault)
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
  map Shift.Rotate index = case index of
    Declaration local -> Shift (Declaration local)
    Pattern bind -> Shift (Pattern bind)
    Shift (Declaration local) -> Declaration local
    Shift (Pattern bind) -> Pattern bind
    Shift (Shift index) -> Shift (Shift index)
  map (Shift.Unshift _) (Shift index) = index
  map (Shift.Unshift abort) _ = absurd abort

data Category scope scope' = Category
  { general :: Shift.Category scope scope',
    term :: Index scope -> Index scope'
  }

class (Shift.Functor term) => Functor term where
  map ::
    Category scope scope' ->
    term scope ->
    term scope'

instance Functor Index where
  map Category {term} = term

mapDefault :: (Functor term) => Shift.Category scope scope' -> term scope -> term scope'
mapDefault general = map Category {general, term = Shift.map general}

over :: Category scopes scopes' -> Category (scope ':+ scopes) (scope ':+ scopes')
over Category {general, term} =
  Category
    { general = Shift.Over general,
      term =
        \case
          Shift index -> Shift (term index)
          Declaration index -> Declaration index
          Pattern index -> Pattern index
    }

data Bound
  = At
  | Select !Int !Bound
  deriving (Show, Read, Eq)
