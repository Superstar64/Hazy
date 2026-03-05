module Stage3.Tree.Do where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage3.Tree.Declarations (Declarations)
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)
import Stage3.Tree.Pattern (Pattern)
import qualified Stage4.Tree.Evidence as Simple (Evidence)

data Do scope
  = Done {done :: !(Expression scope)}
  | Run
      { evidence :: !(Simple.Evidence scope),
        effect :: !(Expression scope),
        after :: !(Do scope)
      }
  | Bind
      { patternx :: !(Pattern scope),
        evidence :: !(Simple.Evidence scope),
        effect :: !(Expression scope),
        thenx :: !(Do (Scope.Pattern ':+ scope)),
        fail :: !Bool
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Do (Scope.Declaration ':+ scope))
      }
  deriving (Show)
