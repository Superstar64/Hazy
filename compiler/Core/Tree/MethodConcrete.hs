module Core.Tree.MethodConcrete where

import qualified Core.Shift as Shift2
import Core.Substitute (Category (Substitute))
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Expression as Expression
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import Core.Tree.SchemeOver (SchemeOver (..))
import qualified Core.Tree.SchemeOver as SchemeOver
import qualified Data.Vector as Vector
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.MethodConcrete as Semantic

newtype MethodConcrete scope = Definition
  { definition :: SchemeOver Expression (Local ':+ scope)
  }
  deriving (Show)

instance Shift MethodConcrete where
  shift = shiftDefault

instance Shift.Functor MethodConcrete where
  map = Shift2.mapDefault

instance Shift2.Functor MethodConcrete where
  map = Substitute.mapDefault

instance Substitute.Functor MethodConcrete where
  map category Definition {definition} =
    Definition
      { definition = Substitute.map (Substitute.Over category) definition
      }

simplify :: Semantic.MethodConcrete Normal Check scope -> MethodConcrete scope
simplify = \case
  Semantic.Definition {definition = Check definition} ->
    Definition
      { definition = SchemeOver.map (SchemeOver.Map Expression.simplify) definition
      }
  Semantic.Default {base = Solved base, self = Solved self, defaultx = Solved defaultx} ->
    Definition
      { definition =
          let typeReplacements = Vector.singleton base
              evidenceReplacements = Vector.singleton self
              category = Substitute Shift.Shift typeReplacements evidenceReplacements
           in Substitute.map category defaultx
      }
