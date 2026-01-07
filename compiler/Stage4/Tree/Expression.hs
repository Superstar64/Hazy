module Stage4.Tree.Expression where

import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage4.Index.Term as Term
import Stage4.Tree.Declarations (Declarations)
import Stage4.Tree.Evidence (Evidence)
import Stage4.Tree.Instanciation (Instanciation)
import Stage4.Tree.Statements (Statements)

data Expression scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Instanciation scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        arguments :: !(Strict.Vector (Expression scope))
      }
  | Selector
      { selector :: !(Selector.Index scope),
        argument :: !(Expression scope)
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Evidence scope),
        instanciation :: !(Instanciation scope)
      }
  | Integer
      { integer :: !Integer
      }
  | Character
      { character :: !Char
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Expression (Scope.Declaration ':+ scope))
      }
  | Lambda
      { body :: !(Expression (Scope.Declaration ':+ scope))
      }
  | Call
      { function :: !(Expression scope),
        argument :: !(Expression scope)
      }
  | Join
      { statements :: !(Statements scope)
      }
  deriving (Show)
