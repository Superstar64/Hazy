module Semantic.Check.Temporary.Field where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Entry (Entry)
import qualified Semantic.Check.Temporary.Entry as Entry
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Field as Semantic
import qualified Semantic.Tree.Field as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Variable (Variable)

data Field s scope = Field
  { position :: !Position,
    name :: !Variable,
    entry :: !(Entry s scope)
  }

check :: Context s scope -> Semantic.Field Resolve scope -> ST s (Field s scope)
check context Semantic.Field {position, name, entry} = do
  entry <- Entry.check context entry
  pure Field {position, name, entry}

solve :: Context s scope -> Field s scope -> Unify.Solve s (Solved.Field Check scope)
solve context Field {position, name, entry} = do
  entry <- Entry.solve context entry
  pure Solved.Field {position, name, entry}
