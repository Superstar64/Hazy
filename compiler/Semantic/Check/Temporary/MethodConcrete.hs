module Semantic.Check.Temporary.MethodConcrete where

import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Type as Simple (Type)
import qualified Core.Tree.TypeLambda as Simple (TypeLambda, TypeLambdaOver)
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition as Solved (Definition)
import qualified Semantic.Tree.MethodConcrete as Solved (MethodConcrete (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data MethodConcrete s scope
  = Definition
      { position :: !Position,
        definition :: !(Unify.Solve s (Simple.TypeLambdaOver (Solved.Definition Group Check) (Local ':+ scope)))
      }
  | Default
      { base :: !(Simple.Type (Local ':+ scope)),
        self :: !(Simple.Evidence (Local ':+ scope)),
        defaultx :: !(Unify.Solve s (Simple.TypeLambda (Local ':+ scope)))
      }

solve :: MethodConcrete s scope -> Unify.Solve s (Solved.MethodConcrete Group Check scope)
solve = \case
  Definition {definition} -> do
    definition <- definition
    pure Solved.Definition {definition = Check definition}
  Default {base, self, defaultx} -> do
    defaultx <- defaultx
    pure
      Solved.Default
        { base = Solved base,
          self = Solved self,
          defaultx = Solved defaultx
        }
