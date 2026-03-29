module Stage3.Tree.Expression where

import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Constructor as Constructor
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage3.Tree.Alternative (Alternative)
import Stage3.Tree.CallHead (CallHead)
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import Stage3.Tree.Declarations (Declarations)
import Stage3.Tree.Do (Do)
import Stage3.Tree.ExpressionField (Field)
import Stage3.Tree.Lambda (Lambda)
import Stage3.Tree.Pattern (Pattern)
import Stage3.Tree.RightHandSide (RightHandSide)
import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation (..))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import Prelude hiding (Bool (False, True))

data Expression scope
  = CallHead
      { callHead :: !(CallHead scope)
      }
  | Record
      { constructor :: !(Constructor.Index scope),
        constructorInfo :: !ConstructorInfo,
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
  | LambdaCase
      { cases :: !(Strict.Vector (Alternative scope))
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide scope))
      }
  | Character {character :: !Char}
  | String {string :: !Text}
  | Do
      { statements :: !(Do scope)
      }
  | Annotation
      { expression :: !(Simple.SchemeOver Expression scope),
        annotation :: !(Scheme scope),
        instanciation :: !(Simple.Instanciation scope)
      }
  deriving (Show)

instance Scope.Show Expression where
  showsPrec = showsPrec
