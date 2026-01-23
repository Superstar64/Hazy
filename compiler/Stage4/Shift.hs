module Stage4.Shift where

import Stage2.Scope (Declaration, Environment (..), Pattern, SimplePattern)
import qualified Stage2.Shift as Stage2
import Prelude hiding (Functor, map)

data Category scope scope' where
  Lift :: Stage2.Category scope scope' -> Category scope scope'
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  ReplaceWildcard :: Category (Pattern ':+ scope) (Declaration ':+ scope)
  SimplifyPattern :: Int -> Category (Pattern ':+ scope) (Pattern ':+ Pattern ':+ scope)
  RenamePattern :: (Int -> Int) -> Category (Pattern ':+ scope) (Pattern ':+ scope)
  SimplifyList :: Category (Pattern ':+ scope) (Pattern ':+ scope)
  LetPattern :: Category (Pattern ':+ scope) (Pattern ':+ Declaration ':+ scope)
  FinishPattern :: Category (Pattern ':+ scope) (SimplePattern ':+ scope)

general :: Category scope scope' -> Stage2.Category scope scope'
general = \case
  Lift category -> category
  Over category -> Stage2.Over (general category)
  ReplaceWildcard -> Stage2.Unshift (error "bad unshift") Stage2.:. Stage2.Over Stage2.Shift
  SimplifyPattern _ -> Stage2.Shift
  RenamePattern _ -> Stage2.Id
  SimplifyList -> Stage2.Id
  LetPattern -> Stage2.Over Stage2.Shift
  FinishPattern -> Stage2.Unshift (error "bad unshift") Stage2.:. Stage2.Over Stage2.Shift

class (Stage2.Functor term) => Functor term where
  map ::
    Category scope scope' ->
    term scope ->
    term scope'

mapDefault :: (Functor term) => Stage2.Category scope scope' -> term scope -> term scope'
mapDefault = map . Lift
