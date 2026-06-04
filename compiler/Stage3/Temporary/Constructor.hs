module Stage3.Temporary.Constructor where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import qualified Stage1.Variable as Variable
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Constructor as Solved
import qualified Stage2.Tree.Constructor as Stage2 (Constructor (..))
import Stage3.Check.Context (Context (..))
import Stage3.Temporary.Entry (Entry)
import qualified Stage3.Temporary.Entry as Entry
import Stage3.Temporary.Field (Field)
import qualified Stage3.Temporary.Field as Field
import qualified Stage3.Unify as Unify

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

check :: Context s scope -> Stage2.Constructor Resolve scope -> ST s (Constructor s scope)
check context constructor = case constructor of
  Stage2.Constructor {position, name, entries} -> do
    entries <- traverse (Entry.check context) entries
    pure Constructor {position, name, entries}
  Stage2.Record {position, name, fields} -> do
    fields <- traverse (Field.check context) fields
    pure Record {position, name, fields}

solve :: Context s scope -> Constructor s scope -> Unify.Solve s (Solved.Constructor Check scope)
solve context Constructor {position, name, entries} = do
  entries <- traverse (Entry.solve context) entries
  pure Solved.Constructor {position, name, entries}
solve context Record {position, name, fields} = do
  fields <- traverse (Field.solve context) fields
  pure $ Solved.Record {position, name, fields}
