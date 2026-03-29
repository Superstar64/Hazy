module Stage3.Tree.Definition2 where

import Stage2.Index.Term (Bound)
import qualified Stage2.Scope as Scope (Show (..))
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Pattern (Pattern)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation)
import qualified Stage4.Tree.Type as Simple (Type)
import Prelude hiding (Maybe (Just))

data Definition2 scope
  = Body
      { definition :: !(Definition scope),
        typex :: !(Simple.Type scope)
      }
  | Shared
      { shareIndex :: !Int,
        instanciation :: !(Simple.Instanciation scope),
        patternx :: !(Pattern scope),
        bound :: !Bound,
        typex :: !(Simple.Type scope)
      }
  deriving (Show)

instance Scope.Show Definition2 where
  showsPrec = showsPrec
