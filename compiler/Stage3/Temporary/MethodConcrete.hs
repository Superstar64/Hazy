module Stage3.Temporary.MethodConcrete where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..), Local)
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Implicit (Implicit (..))
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.MethodConcrete as Solved
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)
import qualified Stage4.Tree.Type as Simple (Type)

data MethodConcrete s scope
  = Definition
      { position :: !Position,
        definition :: !(Unify.SchemeOver Definition s (Local ':+ scope))
      }
  | Default
      { base :: !(Simple.Type (Local ':+ scope)),
        self :: !(Simple.Evidence (Local ':+ scope)),
        defaultx :: !(Unify.Delay (Simple.SchemeOver Simple.Expression) s (Local ':+ scope))
      }

solve :: MethodConcrete s scope -> ST s (Solved.MethodConcrete Group Check scope)
solve = \case
  Definition {position, definition} -> do
    definition <- Unify.solveSchemeOver (Unify.SolveScheme $ const Definition.solve) position definition
    pure Solved.Definition {definition = Check definition}
  Default {base, self, defaultx = Unify.Delay defaultx} -> do
    defaultx <- defaultx
    pure
      Solved.Default
        { base = Solved base,
          self = Solved self,
          defaultx = Solved defaultx
        }
