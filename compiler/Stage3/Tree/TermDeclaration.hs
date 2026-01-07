module Stage3.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Local)
import qualified Stage3.Simple.Scheme as Simple (Scheme, mono)
import qualified Stage3.Simple.Type as Simple (Type)
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Scheme (Scheme)
import Prelude hiding (Maybe (Just))

data TermDeclaration scope
  = Manual
      { name :: !Variable,
        definition :: !(Definition (Scope.Local ':+ scope)),
        annotation :: !(Scheme scope),
        typex :: !(Simple.Scheme scope)
      }
  | Auto
      { name :: !Variable,
        definitionAuto :: !(Definition scope),
        typeAuto :: !(Simple.Type scope)
      }
  deriving (Show)

simple :: TermDeclaration scope -> Simple.Scheme scope
simple = \case
  Auto {typeAuto} -> Simple.mono typeAuto
  Manual {typex} -> typex
