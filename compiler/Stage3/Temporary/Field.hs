module Stage3.Temporary.Field where

import Control.Monad.ST (ST)
import Stage1.Variable (Variable)
import qualified Stage2.Tree.Field as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Entry (Entry)
import qualified Stage3.Temporary.Entry as Entry
import qualified Stage3.Tree.Field as Solved

data Field s scope = Field
  { name :: !Variable,
    entry :: !(Entry s scope)
  }

check :: Context s scope -> Stage2.Field scope -> ST s (Field s scope)
check context Stage2.Field {name, entry} = do
  entry <- Entry.check context entry
  pure Field {name, entry}

solve :: Synonym.Context s scope -> Field s scope -> ST s (Solved.Field scope)
solve context Field {name, entry} = do
  entry <- Entry.solve context entry
  pure Solved.Field {name, entry}
