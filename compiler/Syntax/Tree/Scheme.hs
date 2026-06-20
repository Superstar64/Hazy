{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.Scheme where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser (Parser, asum, many, position, token)
import Syntax.Position (Position)
import Syntax.Tree.Constraints (Constraints)
import qualified Syntax.Tree.Constraints as Constraints
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type
import Syntax.Tree.TypePattern (TypePattern)
import qualified Syntax.Tree.TypePattern as TypePattern

data Scheme position
  = -- |
    -- > forall x. C y => t
    Explicit
      { startPosition :: !position,
        parameters :: !(Strict.Vector (TypePattern position)),
        constraints :: !(Constraints position),
        result :: !(Type position)
      }
  | -- |
    -- > C x => t
    Implicit
      { startPosition :: !position,
        constraints :: !(Constraints position),
        result :: !(Type position)
      }
  deriving (Show)

mono :: Type position -> Scheme position
mono result =
  Implicit
    { startPosition = Type.startPosition result,
      constraints = Constraints.None,
      result
    }

parse :: Parser (Scheme Position)
parse =
  asum
    [ explicit <$> position <*> forallx <*> Constraints.parse <*> Type.parse,
      implicit <$> position <*> Constraints.parse <*> Type.parse
    ]
  where
    explicit startPosition parameters constraints result =
      Explicit
        { startPosition,
          parameters,
          constraints,
          result
        }
    implicit startPosition constraints result =
      Implicit
        { startPosition,
          constraints,
          result
        }
    forallx = token "forall" *> (Strict.Vector.fromList <$> many TypePattern.parse) <* token "."
