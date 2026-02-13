module Stage4.Shift (Category (..), Functor (..), mapDefault, mapInstances) where

import qualified Data.Map as Map
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Pattern, SimpleDeclaration, SimplePattern)
import qualified Stage2.Shift as Stage2
import qualified Stage3.Index.Evidence as Evidence
import Prelude hiding (Functor, map)

data Category scope scope' where
  Lift :: Stage2.Category scope scope' -> Category scope scope'
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  ReplaceWildcard :: Category (Pattern ':+ scope) (SimpleDeclaration ':+ scope)
  SimplifyPattern :: Int -> Category (Pattern ':+ scope) (Pattern ':+ Pattern ':+ scope)
  RenamePattern :: (Int -> Int) -> Category (Pattern ':+ scope) (Pattern ':+ scope)
  SimplifyList :: Category (Pattern ':+ scope) (Pattern ':+ scope)
  LetPattern :: Category (Pattern ':+ scope) (Pattern ':+ SimpleDeclaration ':+ scope)
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

instance Functor Local.Index where
  map = Stage2.map . general

instance Functor Type.Index where
  map = Stage2.map . general

instance Functor Type2.Index where
  map = Stage2.map . general

instance Functor Constructor.Index where
  map = Stage2.map . general

instance Functor Selector.Index where
  map = Stage2.map . general

instance Functor Method.Index where
  map = Stage2.map . general

instance Functor Evidence.Index where
  map = Stage2.map . general

mapInstances ::
  Category scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor term) => Stage2.Category scope scope' -> term scope -> term scope'
mapDefault = map . Lift
