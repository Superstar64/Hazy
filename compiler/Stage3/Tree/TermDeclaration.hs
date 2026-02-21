module Stage3.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import Stage2.Index.Term (Bound)
import qualified Stage2.Scope as Scope (Show (..))
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Pattern (Pattern)
import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation)
import qualified Stage4.Tree.Scheme as Simple (Scheme (Scheme))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import qualified Stage4.Tree.Type as Simple (Type)
import Prelude hiding (Maybe (Just))

data LazyTermDeclaration scope = !Variable :^ TermDeclaration scope
  deriving (Show)

infix 4 :^

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

strict declaration = name declaration :^ declaration

data Body scope
  = Body
      { definition :: !(Definition scope),
        typex :: !(Simple.Type scope)
      }
  | Shared
      { shareIndex :: !Int,
        instanciation :: !(Simple.Instanciation scope),
        patternx :: !(Pattern scope),
        bound :: !Bound,
        typex :: !(Simple.Type scope)
      }
  deriving (Show)

instance Scope.Show Body where
  showsPrec = showsPrec

simple :: TermDeclaration scope -> Simple.Scheme scope
simple declaration = case body declaration of
  Simple.SchemeOver {parameters, constraints, result}
    | result <- typex result ->
        Simple.Scheme
          Simple.SchemeOver
            { parameters,
              constraints,
              result
            }
