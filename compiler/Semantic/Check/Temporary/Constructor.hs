module Semantic.Check.Temporary.Constructor where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Context (Context (..))
import Semantic.Check.Temporary.Entry (Entry)
import qualified Semantic.Check.Temporary.Entry as Entry
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Tree.Constructor as Semantic (Constructor (..))
import qualified Semantic.Tree.Constructor as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import qualified Syntax.Variable as Variable

data Constructor s scope
  = Constructor
  { position :: !Position,
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (Entry s scope))
  }

check :: Context s scope -> Semantic.Constructor Resolve scope -> ST s (Constructor s scope)
check context constructor = case constructor of
  Semantic.Constructor {position, name, syntax, entries} -> do
    entries <- traverse (Entry.check context) entries
    pure Constructor {position, name, syntax, entries}

solve :: Context s scope -> Constructor s scope -> Unify.Solve s (Solved.Constructor Check scope)
solve context Constructor {position, name, syntax, entries} = do
  entries <- traverse (Entry.solve context) entries
  pure Solved.Constructor {position, name, syntax, entries}
