module Stage3.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import qualified Stage2.Scope as Scope (Show (..))
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Simple (Scheme (Scheme))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import qualified Stage4.Tree.Type as Simple (Type)
import Prelude hiding (Maybe (Just))

data TermDeclaration scope
  = Manual
      { name :: !Variable,
        body :: !(Simple.SchemeOver Body scope),
        annotation :: !(Scheme scope)
      }
  | Auto
      { name :: !Variable,
        body :: !(Simple.SchemeOver Body scope)
      }
  deriving (Show)

data Body scope = Body
  { definition :: !(Definition scope),
    typex :: !(Simple.Type scope)
  }
  deriving (Show)

instance Scope.Show Body where
  showsPrec = showsPrec

simple :: TermDeclaration scope -> Simple.Scheme scope
simple declaration = case body declaration of
  Simple.SchemeOver {parameters, constraints, result = Body {typex}} ->
    Simple.Scheme
      Simple.SchemeOver
        { parameters,
          constraints,
          result = typex
        }
