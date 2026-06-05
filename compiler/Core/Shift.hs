module Core.Shift (Category (..), Functor (..), mapDefault, mapInstances) where

import qualified Data.Map as Map
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Pattern, SimpleDeclaration, SimplePattern)
import qualified Semantic.Shift as Semantic
import Prelude hiding (Functor, map)

data Category scope scope' where
  Lift :: Semantic.Category scope scope' -> Category scope scope'
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  ReplaceWildcard :: Category (Pattern ':+ scope) (SimpleDeclaration ':+ scope)
  SimplifyPattern :: Int -> Category (Pattern ':+ scope) (Pattern ':+ Pattern ':+ scope)
  RenamePattern :: (Int -> Int) -> Category (Pattern ':+ scope) (Pattern ':+ scope)
  SimplifyList :: Category (Pattern ':+ scope) (Pattern ':+ scope)
  LetPattern :: Category (Pattern ':+ scope) (Pattern ':+ SimpleDeclaration ':+ scope)
  FinishPattern :: Category (Pattern ':+ scope) (SimplePattern ':+ scope)
  FinishNewtype :: Category (SimplePattern ':+ scope) (SimpleDeclaration ':+ scope)
  ReplaceIrrefutable ::
    Int ->
    Category (SimplePattern ':+ scope) (SimplePattern ':+ (SimpleDeclaration ':+ scope))

general :: Category scope scope' -> Semantic.Category scope scope'
general = \case
  Lift category -> category
  Over category -> Semantic.Over (general category)
  ReplaceWildcard -> Semantic.Unshift (error "bad unshift") Semantic.:. Semantic.Over Semantic.Shift
  SimplifyPattern _ -> Semantic.Shift
  RenamePattern _ -> Semantic.Id
  SimplifyList -> Semantic.Id
  LetPattern -> Semantic.Over Semantic.Shift
  FinishPattern -> Semantic.Unshift (error "bad unshift") Semantic.:. Semantic.Over Semantic.Shift
  FinishNewtype -> Semantic.Unshift (error "bad unshift") Semantic.:. Semantic.Over Semantic.Shift
  ReplaceIrrefutable _ -> Semantic.Over Semantic.Shift

class (Semantic.Functor term) => Functor term where
  map ::
    Category scope scope' ->
    term scope ->
    term scope'

instance Functor Local.Index where
  map = Semantic.map . general

instance Functor Type.Index where
  map = Semantic.map . general

instance Functor Type2.Index where
  map = Semantic.map . general

instance Functor Constructor.Index where
  map = Semantic.map . general

instance Functor Selector.Index where
  map = Semantic.map . general

instance Functor Method.Index where
  map = Semantic.map . general

instance Functor Evidence0.Index where
  map = Semantic.map . general

instance Functor Evidence.Index where
  map = Semantic.map . general

mapInstances ::
  Category scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor term) => Semantic.Category scope scope' -> term scope -> term scope'
mapDefault = map . Lift
