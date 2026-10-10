module Semantic.Check.Binding.Term where

import Control.Monad.ST (ST)
import qualified Core.Tree.Forall as Core (mono)
import qualified Core.Tree.Forall as Simple (Forall)
import qualified Core.Type.Functor as Core (mapLogical, shiftLogical)
import Data.Void (Void)
import Semantic.Scope (Environment (..), GroupTerm)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Declaration (Declaration (..))
import qualified Semantic.Unify as Unify

data Type s scope
  = Wobbly !(Unify.Forall s scope)
  | Rigid !(Simple.Forall scope)

newtype TermBinding s scope = TermBinding
  {typex :: ST s (Type s scope)}

instance Shift0.Functor (Type s) where
  map category = \case
    Wobbly typex -> Wobbly (Core.mapLogical category typex)
    Rigid typex -> Rigid (Shift0.map category typex)

instance Shift0.Functor (TermBinding s) where
  map category TermBinding {typex} = TermBinding {typex = fmap (Shift0.map category) typex}

global :: Declaration solve Void locality (ST s) layout Check scope -> TermBinding s scope
global Declaration {typex} = TermBinding $ do
  typex <- typex
  pure $ Rigid $ case typex of
    Solved typex -> typex

local :: Declaration solve (Unify.Logical s scope) locality (ST s) layout Check scope -> TermBinding s scope
local Declaration {typex} = TermBinding $ do
  typex <- typex
  pure $ Wobbly $ case typex of
    Solved typex -> typex

group :: Unify.Type s scopes -> TermBinding s (GroupTerm ':+ scopes)
group = TermBinding . pure . Wobbly . Core.mono . Core.shiftLogical
