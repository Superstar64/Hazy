module Stage3.Tree.TermDeclaration where

import Stage1.Variable (Variable)
import Stage3.Tree.Definition2 (Definition2 (..))
import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Simple (Scheme (Scheme))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import Prelude hiding (Maybe (Just))

data LazyTermDeclaration scope = !Variable :^ TermDeclaration scope
  deriving (Show)

infix 4 :^

data TermDeclaration scope
  = Manual
      { name :: !Variable,
        body :: !(Simple.SchemeOver Definition2 scope),
        annotation :: !(Scheme scope)
      }
  | Auto
      { name :: !Variable,
        body :: !(Simple.SchemeOver Definition2 scope)
      }
  deriving (Show)

strict declaration = name declaration :^ declaration

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
