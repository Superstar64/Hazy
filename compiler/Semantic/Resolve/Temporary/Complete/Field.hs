{-# LANGUAGE_HAZY UnorderedRecords #-}
module Semantic.Resolve.Temporary.Complete.Field where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Entry as Entry
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Field as Real
import Syntax.Position (Position)
import qualified Syntax.Tree.Field as Syntax
import Syntax.Tree.Marked (Marked ((:@)))
import Syntax.Variable (Variable)

data Field scope = Field
  { position :: !Position,
    name :: !Variable,
    field :: Real.Field Resolve scope
  }

shrink :: Field scope -> Real.Field Resolve scope
shrink = field

indexes :: [Field scope] -> Map Variable Int
indexes fields = Map.fromList $ zip [name field | field <- fields] [0 ..]

resolve :: Context scope -> Syntax.Field Position -> [(Variable, Field scope)]
resolve context Syntax.Field {names, entry} = do
  position :@ name <- toList names
  let item =
        Field
          { position,
            name,
            field =
              Real.Field
                { position,
                  name,
                  entry = Entry.resolve context entry
                }
          }
  pure (name, item)
