module Stage3.Temporary.Constructor where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Tree.Constructor as Stage2 (Constructor (..))
import qualified Stage2.Tree.Constructor as Stage2.Constructor
import Stage3.Check.Context (Context (..))
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Entry (Entry)
import qualified Stage3.Temporary.Entry as Entry
import Stage3.Temporary.Field (Field)
import qualified Stage3.Temporary.Field as Field
import qualified Stage3.Tree.Constructor as Solved

data Constructor s scope
  = Constructor
      { entries :: !(Strict.Vector (Entry s scope))
      }
  | Record
      { fields :: !(Strict.Vector (Field s scope))
      }

check :: Context s scope -> Stage2.Constructor scope -> ST s (Constructor s scope)
check context constructor = case constructor of
  Stage2.Constructor {entries} -> do
    entries <- traverse (Entry.check context) entries
    pure Constructor {entries}
  Stage2.Record {fields} -> do
    fields <- traverse (Field.check context) fields
    pure Record {fields}

solve :: Synonym.Context s scope -> Constructor s scope -> ST s (Solved.Constructor scope)
solve context Constructor {entries} = do
  entries <- traverse (Entry.solve context) entries
  pure Solved.Constructor {entries}
solve context Record {fields} = do
  fields <- traverse (Field.solve context) fields
  pure $ Solved.Record {fields}
