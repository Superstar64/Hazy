module Stage3.Tree.Declaration where

import Stage1.Variable (Variable)
import Stage2.Tree.Declaration (Key (..))
import Stage2.Tree.Definition2 (Share, Single)
import Stage3.Tree.Definition2 (Definition2)
import qualified Stage3.Tree.Definition2 as Definition2
import Stage3.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Simple (Scheme (Scheme))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import Prelude hiding (Maybe (Just))

data LazyTermDeclaration scope = !Key :^ Declaration scope
  deriving (Show)

infix 4 :^

data Declaration scope
  = Annotated
      { name :: !Variable,
        body :: !(Simple.SchemeOver (Definition2 Single) scope),
        annotation :: !(Scheme scope)
      }
  | Inferred
      { name :: !Variable,
        body :: !(Simple.SchemeOver (Definition2 Single) scope)
      }
  | Shared
      { index :: !Int,
        body' :: !(Simple.SchemeOver (Definition2 Share) scope)
      }
  deriving (Show)

key :: Declaration scope -> Key
key = \case
  Annotated {name} -> Named name
  Inferred {name} -> Named name
  Shared {index} -> Unnamed index

strict :: Declaration scope -> LazyTermDeclaration scope
strict declaration = key declaration :^ declaration

simple :: Declaration scope -> Simple.Scheme scope
simple = \case
  Annotated {body} -> go body
  Inferred {body} -> go body
  Shared {body'} -> go body'
  where
    go :: Simple.SchemeOver (Definition2 source) scope -> Simple.Scheme scope
    go Simple.SchemeOver {parameters, constraints, result}
      | result <- Definition2.typex result =
          Simple.Scheme
            Simple.SchemeOver
              { parameters,
                constraints,
                result
              }
