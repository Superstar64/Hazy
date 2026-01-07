module Stage4.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import Stage4.Tree.Scheme (Scheme)

data TermDeclaration scope = Definition
  { name :: !Variable,
    definition :: !(Expression (Scope.Local ':+ scope)),
    typex :: !(Scheme scope)
  }
  deriving (Show)
