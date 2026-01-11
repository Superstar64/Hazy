module Stage2.Temporary.Complete.GADTConstructor where

import Data.Foldable (toList)
import Stage1.Position (Position)
import qualified Stage1.Tree.GADTConstructor as Stage1
import Stage1.Tree.Marked (Marked ((:@)))
import Stage1.Variable (Constructor)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Tree.GADTConstructor as Real
import qualified Stage2.Tree.Scheme as Scheme

data GADTConstructor scope = GADTConstructor
  { position :: !Position,
    name :: !Constructor,
    constructor :: Real.GADTConstructor scope
  }
  deriving (Show)

shrink :: GADTConstructor scope -> Real.GADTConstructor scope
shrink = constructor

resolve :: Context scope -> Stage1.GADTConstructor -> [GADTConstructor scope]
resolve context Stage1.GADTConstructor {names, scheme} = do
  position :@ name <- toList names
  pure
    GADTConstructor
      { position,
        name,
        constructor =
          Real.GADTConstructor
            { position,
              name,
              typex = Scheme.resolve context scheme
            }
      }
