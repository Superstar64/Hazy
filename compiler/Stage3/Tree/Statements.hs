module Stage3.Tree.Statements where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage3.Tree.Declarations (Declarations)
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)
import Stage3.Tree.Pattern (Pattern)

data Statements scope
  = Done {done :: !(Expression scope)}
  | Run
      { check :: !(Expression scope),
        after :: !(Statements scope)
      }
  | Bind
      { patternx :: !(Pattern scope),
        check :: !(Expression scope),
        thenx :: !(Statements (Scope.Pattern ':+ scope))
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        body :: !(Statements (Scope.Declaration ':+ scope))
      }
  deriving (Show)
