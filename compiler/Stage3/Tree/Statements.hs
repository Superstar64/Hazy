module Stage3.Tree.Statements (Statements (..), Evidence (..), Syntax, Guard, Do, Equal (..), IsDo (..)) where

import Stage2.Layout (Normal)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Inferred (Inferred)
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.Statements (Do, Equal (..), Evidence (..), Guard, IsDo (..), Syntax)
import qualified Stage2.Tree.Statements as Stage2
import Stage3.Tree.Declarations (Declarations)
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)

data Statements syntax scope
  = Done {done :: !(Expression scope)}
  | Run
      { evidence :: !(Inferred (Stage2.Evidence syntax) Check scope),
        check :: !(Expression scope),
        after :: !(Statements syntax scope)
      }
  | Bind
      { patternx :: !(Pattern Check scope),
        evidence :: !(Inferred (Stage2.Evidence syntax) Check scope),
        check :: !(Expression scope),
        thenx :: !(Statements syntax (Scope.Pattern ':+ scope)),
        fail :: !Bool
      }
  | Let
      { declarations :: !(Declarations Local Normal (Scope.Declaration ':+ scope)),
        body :: !(Statements syntax (Scope.Declaration ':+ scope))
      }
  deriving (Show)
