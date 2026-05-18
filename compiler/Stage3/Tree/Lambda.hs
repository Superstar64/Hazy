module Stage3.Tree.Lambda where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check)
import Stage2.Tree.Pattern (Pattern)
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)

data Lambda scope
  = Plain
      { plain :: !(Expression scope)
      }
  | Bound
      { parameter :: !(Pattern Check scope),
        body :: Lambda (Scope.Pattern ':+ scope)
      }
  deriving (Show)
