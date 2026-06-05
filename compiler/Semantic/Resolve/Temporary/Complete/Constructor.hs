module Semantic.Resolve.Temporary.Complete.Constructor where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateFieldEntries)
import Order (orderList')
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Entry as Entry
import Semantic.Resolve.Temporary.Complete.Field (Field)
import qualified Semantic.Resolve.Temporary.Complete.Field as Field
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Constructor as Real
import Syntax.Position (Position)
import qualified Syntax.Tree.Constructor as Syntax
import Syntax.Tree.Marked (Marked ((:@)))
import Syntax.Variable (Variable)
import qualified Syntax.Variable as Variable (Constructor)

data Constructor scope
  = Constructor
  { position :: !Position,
    name :: !Variable.Constructor,
    fields :: !(Strict.Vector (Field scope)),
    selections :: Strict.Vector (Strict.Maybe Int),
    constructor :: Real.Constructor Resolve scope
  }

shrink = constructor

resolve :: Context scope -> Strict.Vector Variable -> Syntax.Constructor Position -> Constructor scope
resolve context selectorNames constructor = case constructor of
  Syntax.Constructor {constructor = position :@ name, entries} ->
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
  Syntax.Infix {left, constructor = position :@ name, right} ->
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
  Syntax.Record {constructor = position :@ name, fields = fields'} ->
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
