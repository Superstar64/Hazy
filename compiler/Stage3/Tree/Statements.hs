module Stage3.Tree.Statements where

import Stage2.Layout (Normal)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Stage (Check)
import Stage2.Tree.Pattern (Pattern)
import Stage3.Tree.Declarations (Declarations)
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)

data Statements scope
  = Done {done :: !(Expression scope)}
  | Run
      { check :: !(Expression scope),
        after :: !(Statements scope)
      }
  | Bind
      { patternx :: !(Pattern Check scope),
        check :: !(Expression scope),
        thenx :: !(Statements (Scope.Pattern ':+ scope))
      }
  | Let
      { declarations :: !(Declarations Local Normal (Scope.Declaration ':+ scope)),
        body :: !(Statements (Scope.Declaration ':+ scope))
      }
  deriving (Show)
