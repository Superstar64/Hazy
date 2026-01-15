{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Tree.Type where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import qualified Stage3.Simple.Type as Simple

data Type scope
  = Variable
      { variable :: !(Local.Index scope)
      }
  | Constructor
      { constructor :: !(Type2.Index scope),
        synonym :: !(Strict.Maybe (Simple.Type (Scope.Local ':+ scope)))
      }
  | Tuple
      { elements :: !(Strict.Vector2 (Type scope))
      }
  | Call
      { function :: !(Type scope),
        argument :: !(Type scope)
      }
  | Function
      { parameter :: !(Type scope),
        result :: !(Type scope)
      }
  | List
      { element :: !(Type scope)
      }
  | LiftedList
      { items :: !(Strict.Vector1 (Type scope))
      }
  | SmallType {}
  | Constraint {}
  deriving (Show)
