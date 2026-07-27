module Semantic.Tree.Combinators.Implicit where

import qualified Core.Tree.TypeLambda as Simple
import Data.Kind (Type)
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Show as Term
import Semantic.Stage (Check, Resolve, Stage)
import Semantic.Term (Term (..))
import Prelude hiding (map)

type Implicit :: (Layout -> Stage -> Environment -> Type) -> Layout -> Stage -> Environment -> Type
data Implicit ast layout stage scope where
  Resolve :: !(ast layout Resolve scope) -> Implicit ast layout Resolve scope
  Check :: !(Simple.TypeLambdaOver (ast layout Check) scope) -> Implicit ast layout Check scope

instance (Term.Show ast) => Show (Implicit ast layout stage scope) where
  showsPrec d = \case
    Resolve ast -> showParen (d > 10) $ showString "Resolve " . Term.showsPrec 11 ast
    Check ast -> showParen (d > 10) $ showString "Check " . showsPrec 11 (Simple.map (Simple.Map Term) ast)

instance (Shift.TermFunctor ast) => Shift0.Functor (Implicit ast layout stace) where
  map = Shift.mapDefault

instance (Shift.TermFunctor ast) => Shift.Functor (Implicit ast layout stage) where
  map category = \case
    Resolve ast -> Resolve (Shift.mapTerm category ast)
    Check ast -> Check (Simple.map (Simple.Map runTerm) $ Shift.map category $ Simple.map (Simple.Map Term) ast)

instance (FreeTermVariables ast) => FreeTermVariables (Implicit ast) where
  freeTermVariables target = \case
    Resolve term -> freeTermVariables target term

instance (Connect ast) => Connect (Implicit ast) where
  connect (Resolve ast) = Resolve (connect ast)
  seperate (Check ast) = Check (Simple.map (Simple.Map seperate) ast)
