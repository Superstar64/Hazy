module Semantic.Resolve.Temporary.Complete.GADTConstructor where

import Data.Foldable (toList)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Scheme as Scheme
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.GADTConstructor as Real
import Syntax.Position (Position)
import qualified Syntax.Tree.GADTConstructor as Syntax
import Syntax.Tree.Marked (Marked ((:@)))
import Syntax.Variable (Constructor)

data GADTConstructor scope = GADTConstructor
  { position :: !Position,
    name :: !Constructor,
    constructor :: Real.GADTConstructor Resolve scope
  }
  deriving (Show)

shrink :: GADTConstructor scope -> Real.GADTConstructor Resolve scope
shrink = constructor

resolve :: Context scope -> Syntax.GADTConstructor -> [GADTConstructor scope]
resolve context Syntax.GADTConstructor {names, scheme} = do
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
