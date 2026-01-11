{-# LANGUAGE_HAZY UnorderedRecords #-}
module Stage2.Temporary.Complete.Field where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Stage1.Position (Position)
import qualified Stage1.Tree.Field as Stage1
import Stage1.Tree.Marked (Marked ((:@)))
import Stage1.Variable (Variable)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Tree.Entry as Entry
import qualified Stage2.Tree.Field as Real

data Field scope = Field
  { position :: !Position,
    name :: !Variable,
    field :: Real.Field scope
  }

shrink :: Field scope -> Real.Field scope
shrink = field

indexes :: [Field scope] -> Map Variable Int
indexes fields = Map.fromList $ zip [name field | field <- fields] [0 ..]

resolve :: Context scope -> Stage1.Field Position -> [(Variable, Field scope)]
resolve context Stage1.Field {names, entry} = do
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
