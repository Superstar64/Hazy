module Stage2.Temporary.Complete.Constructor where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateFieldEntries)
import Order (orderList')
import Stage1.Position (Position)
import qualified Stage1.Tree.Constructor as Stage1
import Stage1.Tree.Marked (Marked ((:@)))
import Stage1.Variable (Variable)
import qualified Stage1.Variable as Variable (Constructor)
import Stage2.Resolve.Context (Context)
import Stage2.Temporary.Complete.Field (Field)
import qualified Stage2.Temporary.Complete.Field as Field
import qualified Stage2.Tree.Constructor as Real
import qualified Stage2.Tree.Entry as Entry

data Constructor scope
  = Constructor
  { position :: !Position,
    name :: !Variable.Constructor,
    fields :: !(Strict.Vector (Field scope)),
    selections :: Strict.Vector (Strict.Maybe Int),
    constructor :: Real.Constructor scope
  }

shrink = constructor

resolve :: Context scope -> Strict.Vector Variable -> Stage1.Constructor Position -> Constructor scope
resolve context selectorNames constructor = case constructor of
  Stage1.Constructor {constructor = position :@ name, entries} ->
    Constructor
      { position,
        name,
        fields = Strict.Vector.empty,
        selections = Strict.Vector.empty,
        constructor =
          Real.Constructor
            { position,
              name,
              entries = Strict.Vector.fromList (Entry.resolve context <$> toList entries)
            }
      }
  Stage1.Infix {left, constructor = position :@ name, right} ->
    Constructor
      { position,
        name,
        fields = Strict.Vector.empty,
        selections = Strict.Vector.empty,
        constructor =
          Real.Constructor
            { position,
              name,
              entries = Strict.Vector.fromList (Entry.resolve context <$> [left, right])
            }
      }
  Stage1.Record {constructor = position :@ name, fields = fields'} ->
    Constructor
      { position,
        name,
        fields,
        selections,
        constructor =
          Real.Record
            { position,
              name,
              fields = fmap Field.shrink fields
            }
      }
    where
      fields = orderList' unique (foldMap (Field.resolve context) fields')
        where
          unique [field] = field
          unique fields = duplicateFieldEntries (Field.position <$> fields)
      selections = fmap selector selectorNames
        where
          selector name = maybe Strict.Nothing Strict.Just (indexes Map.!? name)
      indexes = Field.indexes (toList fields)
