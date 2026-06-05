module Semantic.Check.Temporary.Constructor where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Context (Context (..))
import Semantic.Check.Temporary.Entry (Entry)
import qualified Semantic.Check.Temporary.Entry as Entry
import Semantic.Check.Temporary.Field (Field)
import qualified Semantic.Check.Temporary.Field as Field
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constructor as Semantic (Constructor (..))
import qualified Semantic.Tree.Constructor as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import qualified Syntax.Variable as Variable

data Constructor s scope
  = Constructor
      { position :: !Position,
        name :: !Variable.Constructor,
        entries :: !(Strict.Vector (Entry s scope))
      }
  | Record
      { position :: !Position,
        name :: !Variable.Constructor,
        fields :: !(Strict.Vector (Field s scope))
      }

check :: Context s scope -> Semantic.Constructor Resolve scope -> ST s (Constructor s scope)
check context constructor = case constructor of
  Semantic.Constructor {position, name, entries} -> do
    entries <- traverse (Entry.check context) entries
    pure Constructor {position, name, entries}
  Semantic.Record {position, name, fields} -> do
    fields <- traverse (Field.check context) fields
    pure Record {position, name, fields}

solve :: Context s scope -> Constructor s scope -> Unify.Solve s (Solved.Constructor Check scope)
solve context Constructor {position, name, entries} = do
  entries <- traverse (Entry.solve context) entries
  pure Solved.Constructor {position, name, entries}
solve context Record {position, name, fields} = do
  fields <- traverse (Field.solve context) fields
  pure $ Solved.Record {position, name, fields}
