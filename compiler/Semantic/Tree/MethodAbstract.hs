module Semantic.Tree.MethodAbstract where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Definition (Definition)

-- todo, figure out how to use Combinators.Implicit for this

data MethodAbstract layout stage scope where
  Abstract :: MethodAbstract layout stage scope
  DefaultResolve ::
    !(Definition layout Resolve scope) ->
    MethodAbstract layout Resolve scope
  DefaultCheck ::
    !(Definition layout Check (Local ':+ scope)) ->
    MethodAbstract layout Check scope

instance Show (MethodAbstract layout stage scope) where
  showsPrec d = \case
    Abstract -> showString "Abstract"
    DefaultResolve definition ->
      showParen (d > 10) $
        showString "DefaultResolve "
          . showsPrec 11 definition
    DefaultCheck definition ->
      showParen (d > 10) $
        showString "DefaultCheck "
          . showsPrec 11 definition

instance Shift0.Functor (MethodAbstract layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (MethodAbstract layout stage) where
  map category = \case
    Abstract -> Abstract
    DefaultResolve definition -> DefaultResolve (Shift.map category definition)
    DefaultCheck definition -> DefaultCheck (Shift.map (Shift.Over category) definition)

instance FreeTermVariables MethodAbstract where
  freeTermVariables target = \case
    Abstract -> []
    DefaultResolve definition -> freeTermVariables target definition

instance Connect MethodAbstract where
  connect = \case
    Abstract -> Abstract
    DefaultResolve definition -> DefaultResolve (connect definition)
  seperate = \case
    Abstract -> Abstract
    DefaultCheck definition -> DefaultCheck (seperate definition)
