module Stage4.Tree.Statements where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage4.Tree.Declarations (Declarations)
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import Stage4.Tree.Pattern (Pattern)

data Statements scope
  = Done {done :: !(Expression scope)}
  | Bind
      { patternx :: !(Pattern scope),
        check :: !(Expression scope),
        thenx :: !(Statements (Scope.SimplePattern ':+ scope))
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Statements (Scope.Declaration ':+ scope))
      }
  | LetOne
      { declaration :: !(Expression scope),
        body :: !(Statements (Scope.Declaration ':+ scope))
      }
  | Branch
      { left :: !(Statements scope),
        right :: !(Statements scope)
      }
  | Bottom
  deriving (Show)
