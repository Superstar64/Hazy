module Stage3.Tree.Expression where

import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector (Index)
import qualified Stage2.Index.Term as Term (Index)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Selector as Selector (Uniform)
import qualified Stage3.Simple.Evidence as Simple (Evidence)
import qualified Stage3.Simple.Instanciation as Simple (Instanciation)
import Stage3.Tree.Alternative (Alternative)
import Stage3.Tree.Declarations (Declarations)
import Stage3.Tree.ExpressionField (Field)
import Stage3.Tree.Lambda (Lambda)
import Stage3.Tree.Pattern (Pattern)
import Stage3.Tree.RightHandSide (RightHandSide)
import Prelude hiding (Bool (False, True))

data Expression scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Simple.Instanciation scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        parameters :: !Int
      }
  | Selector
      { selector :: !(Selector.Index scope),
        uniform :: !Selector.Uniform
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Simple.Evidence scope),
        instanciation :: !(Simple.Instanciation scope)
      }
  | Record
      { constructor :: !(Constructor.Index scope),
        parameters :: !Int,
        fields :: !(Strict.Vector (Field scope))
      }
  | Integer
      { integer :: !Integer,
        evidence :: !(Simple.Evidence scope)
      }
  | Tuple {elements :: !(Strict.Vector2 (Expression scope))}
  | List {items :: !(Strict.Vector (Expression scope))}
  | Call
      { function :: !(Expression scope),
        argument :: !(Expression scope)
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Expression (Scope.Declaration ':+ scope))
      }
  | If
      { condition :: !(Expression scope),
        thenx :: !(Expression scope),
        elsex :: !(Expression scope)
      }
  | Case
      { scrutinee :: !(Expression scope),
        cases :: !(Strict.Vector (Alternative scope))
      }
  | Lambda
      { parameter :: !(Pattern scope),
        body :: !(Lambda (Scope.Pattern ':+ scope))
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide scope))
      }
  | Character {character :: !Char}
  | String {string :: !Text}
  deriving (Show)
