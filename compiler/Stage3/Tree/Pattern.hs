module Stage3.Tree.Pattern where

import qualified Data.Strict.Vector1 as Strict (Vector1)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Constructor as Constructor
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import Stage3.Tree.PatternField (Field)
import Stage4.Tree.Evidence (Evidence)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern scope
  = Wildcard {}
  | Constructor
      { irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern scope)),
        constructorInfo :: !(ConstructorInfo scope)
      }
  | Record
      { irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope)),
        constructorInfo :: !(ConstructorInfo scope)
      }
  | List
      { irrefutable :: !Prelude.Bool,
        items :: !(Strict.Vector1 (Pattern scope))
      }
  | Integer
      { irrefutable :: !Prelude.Bool,
        integer :: !Integer,
        evidence :: !(Evidence scope),
        equal :: !(Evidence scope)
      }
  | Float
      { irrefutable :: !Prelude.Bool,
        float :: !Rational,
        evidence :: !(Evidence scope),
        equal :: !(Evidence scope)
      }
  | Character
      { irrefutable :: !Prelude.Bool,
        character :: !Char
      }
  | String
      { irrefutable :: !Prelude.Bool,
        string :: !Text
      }
  deriving (Show)
