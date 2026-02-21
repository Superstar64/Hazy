module Stage3.Tree.Pattern where

import qualified Data.Strict.Vector1 as Strict (Vector1)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Constructor as Constructor
import Stage3.Tree.PatternField (Field)
import Stage4.Tree.Evidence (Evidence)
import Prelude hiding (Bool (False, True))
import qualified Prelude

newtype Pattern scope
  = At (Match scope)
  deriving (Show)

data Match scope
  = Wildcard
  | Match {match :: !(Bindings scope), irrefutable :: !Prelude.Bool}
  deriving (Show)

data Bindings scope
  = Constructor
      { constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern scope))
      }
  | Record
      { constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope)),
        fieldCount :: !Int
      }
  | List {items :: !(Strict.Vector1 (Pattern scope))}
  | Integer
      { integer :: !Integer,
        evidence :: !(Evidence scope),
        equal :: !(Evidence scope)
      }
  | Character {character :: !Char}
  | String {text :: !Text}
  deriving (Show)
