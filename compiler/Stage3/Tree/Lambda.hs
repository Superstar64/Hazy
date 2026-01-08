module Stage3.Tree.Lambda where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)
import Stage3.Tree.Pattern (Pattern)

data Lambda scope
  = Plain
      { plain :: !(Expression scope)
      }
  | Bound
      { parameter :: !(Pattern scope),
        body :: Lambda (Scope.Pattern ':+ scope)
      }
  deriving (Show)
