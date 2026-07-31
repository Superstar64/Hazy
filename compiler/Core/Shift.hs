module Core.Shift (Category (..), Functor (..), mapDefault, mapInstances) where

import qualified Data.Map as Map
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Pattern, SimpleDeclaration, SimplePattern)
import Semantic.Shift (shift)
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

instance Functor Term.Index where
  map (Lift category) index = Semantic.map category index
  map (Over category) (Term.Shift index) = Term.Shift $ map category index
  map (Over _) (Term.Declaration index) = Term.Declaration index
  map (Over _) (Term.Pattern bound) = Term.Pattern bound
  map (Over _) (Term.SimplePattern index) = Term.SimplePattern index
  map (Over _) Term.SimpleDeclaration = Term.SimpleDeclaration
  map (Over _) (Term.Group index) = Term.Group index
  map ReplaceWildcard index = case index of
    Term.Pattern Term.At -> Term.SimpleDeclaration
    Term.Pattern _ -> error "bad wildcard bind"
    Term.Shift index -> Term.Shift index
  map (SimplifyPattern target) index = case index of
    Term.Pattern (Term.Select source bound)
      | source == target -> Term.Pattern bound
    Term.Pattern _ -> shift index
    Term.Shift _ -> shift index
  map (RenamePattern rename) index = case index of
    Term.Pattern Term.At -> Term.Pattern Term.At
    Term.Pattern (Term.Select index' bound) ->
      Term.Pattern (Term.Select (rename index') bound)
    Term.Shift index -> Term.Shift index
  map SimplifyList index = case index of
    Term.Pattern Term.At -> Term.Pattern Term.At
    Term.Pattern (Term.Select 0 bound) -> Term.Pattern (Term.Select 0 bound)
    Term.Pattern (Term.Select index bound) ->
      Term.Pattern (Term.Select 1 (Term.Select (index - 1) bound))
    Term.Shift index -> Term.Shift index
  map LetPattern index = case index of
    Term.Pattern Term.At -> Term.Shift Term.SimpleDeclaration
    Term.Pattern (Term.Select index bound) ->
      Term.Pattern (Term.Select index bound)
    Term.Shift index -> Term.Shift (Term.Shift index)
  map FinishPattern index = case index of
    Term.Pattern (Term.Select index Term.At) -> Term.SimplePattern index
    Term.Pattern _ -> error "bad finish pattern"
    Term.Shift index -> Term.Shift index
  map FinishNewtype index = case index of
    Term.SimplePattern n
      | n == 0 -> Term.SimpleDeclaration
      | otherwise -> error "bad finish newtype"
    Term.Shift index -> Term.Shift index
  map (ReplaceIrrefutable target) index = case index of
    Term.SimplePattern index
      | index == target -> Term.Shift Term.SimpleDeclaration
      | otherwise -> Term.SimplePattern index
    Term.Shift index -> Term.Shift (Term.Shift index)

mapInstances ::
  Category scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor term) => Semantic.Category scope scope' -> term scope -> term scope'
mapDefault = map . Lift
