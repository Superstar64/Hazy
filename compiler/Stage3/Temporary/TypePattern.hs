module Stage3.Temporary.TypePattern where

import Control.Monad.ST (ST)
import Stage1.Lexer (VariableIdentifier)
import Stage1.Position (Position)
import qualified Stage3.Tree.TypePattern as Solved
import qualified Stage3.Unify as Unify

data TypePattern s scope = TypePattern
  { position :: !Position,
    typex :: !(Unify.Type s scope),
    name :: !VariableIdentifier
  }

solve :: TypePattern s scope -> ST s (Solved.TypePattern scope)
solve TypePattern {typex, name, position} = do
  typex <- Unify.solve position typex
  pure
    Solved.TypePattern
      { name,
        typex
      }
