module Stage2.Check.Temporary.Field where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Field as Solved
import qualified Stage2.Tree.Field as Stage2
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Entry (Entry)
import qualified Stage2.Check.Temporary.Entry as Entry
import qualified Stage2.Unify as Unify

data Field s scope = Field
  { position :: !Position,
    name :: !Variable,
    entry :: !(Entry s scope)
  }

check :: Context s scope -> Stage2.Field Resolve scope -> ST s (Field s scope)
check context Stage2.Field {position, name, entry} = do
  entry <- Entry.check context entry
  pure Field {position, name, entry}

solve :: Context s scope -> Field s scope -> Unify.Solve s (Solved.Field Check scope)
solve context Field {position, name, entry} = do
  entry <- Entry.solve context entry
  pure Solved.Field {position, name, entry}
