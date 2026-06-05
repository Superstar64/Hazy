module Semantic.Check.Temporary.MethodConcrete where

import qualified Core.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Core.Tree.Expression as Simple (Expression)
import qualified Core.Tree.SchemeOver as Simple (SchemeOver)
import qualified Core.Tree.Type as Simple (Type)
import Semantic.Check.Temporary.Definition (Definition)
import qualified Semantic.Check.Temporary.Definition as Definition
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.MethodConcrete as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data MethodConcrete s scope
  = Definition
      { position :: !Position,
        definition :: !(Unify.SchemeOver Definition s (Local ':+ scope))
      }
  | Default
      { base :: !(Simple.Type (Local ':+ scope)),
        self :: !(Simple.Evidence (Local ':+ scope)),
        defaultx :: !(Unify.Solve s (Simple.SchemeOver Simple.Expression (Local ':+ scope)))
      }

solve :: MethodConcrete s scope -> Unify.Solve s (Solved.MethodConcrete Group Check scope)
solve = \case
  Definition {position, definition} -> do
    definition <- Unify.solveSchemeOver (Unify.SolveScheme $ const Definition.solve) position definition
    pure Solved.Definition {definition = Check definition}
  Default {base, self, defaultx} -> do
    defaultx <- defaultx
    pure
      Solved.Default
        { base = Solved base,
          self = Solved self,
          defaultx = Solved defaultx
        }
