module Stage2.Group.Temporary.Declaration where

import Stage2.FreeVariables (FreeTermVariables (..), Target (..))
import qualified Stage2.Group.Index.Term0 as Term0
import Stage2.Layout (Normal)
import qualified Stage2.Tree.Declaration as Proper (Declaration, freeGroupTermVariables)
import qualified Stage2.Tree.Shared as Proper (Shared)

data Declaration locality scope
  = Declaration !(Proper.Declaration locality Normal scope)
  | Shared !(Proper.Shared locality scope)

freeGroupTermVariables :: Declaration locality scope -> [Term0.Index scope]
freeGroupTermVariables = \case
  Declaration declaration -> map Term0.Index $ Proper.freeGroupTermVariables declaration
  Shared shared -> map Term0.Share $ freeTermVariables Target shared
