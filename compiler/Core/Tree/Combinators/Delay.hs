module Core.Tree.Combinators.Delay where

import Core.Instanciate (Instanciate, Instanciated, Normal)
import qualified Core.Substitute as Substitute
import qualified Core.Type.Show as Core
import qualified Data.Kind as Kind
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type Delay ::
  (Kind.Type -> Environment -> Kind.Type) ->
  Instanciate ->
  Kind.Type ->
  Environment ->
  Kind.Type
data Delay typef instanciate logical scope where
  Delay :: !(typef logical scope) -> Delay typef Instanciated logical scope
  Here :: Delay typef Normal logical scope

instance (Core.Show typef, Show logical) => Show (Delay typef instanciate logical scope) where
  showsPrec k = \case
    Delay typex -> showParen (k > 10) $ showString "Delay " . Core.showsPrec 11 typex
    Here -> showString "Here"

instance (Shift.Functor (typef logical)) => Shift0.Functor (Delay typef instanciate logical) where
  map = Shift.mapDefault

instance (Shift.Functor (typef logical)) => Shift.Functor (Delay typef instanciate logical) where
  map category = \case
    Delay typex -> Delay (Shift.map category typex)
    Here -> Here

instance (Substitute.Functor (typef logical)) => Substitute.Functor (Delay typef instanciate logical) where
  map category = \case
    Delay typex -> Delay (Substitute.map category typex)
    Here -> Here

instance (Substitute.TypeFunctor typef) => Substitute.TypeFunctor (Delay typef instanciate) where
  mapType category = \case
    Delay typex -> Delay (Substitute.mapType category typex)
    Here -> Here
