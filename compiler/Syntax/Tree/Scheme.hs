{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.Scheme where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser (Parser, asum, betweenParens, many, position, sepByComma, token, try, (<**>))
import Syntax.Position (Position)
import Syntax.Tree.Constraint (Constraint)
import qualified Syntax.Tree.Constraint as Constraint
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
        constraints :: !(Strict.Vector (Constraint position)),
        result :: !(Type position)
      }
  | -- |
    -- > C x => t
    Implicit
      { startPosition :: !position,
        constraints :: !(Strict.Vector (Constraint position)),
        result :: !(Type position)
      }
  deriving (Show)

mono :: Type position -> Scheme position
mono result =
  Implicit
    { startPosition = Type.startPosition result,
      constraints = Strict.Vector.empty,
      result
    }

parse :: Parser (Scheme Position)
parse =
  let parseConstraint constrain mono =
        asum
          [ constrain
              <$> try (betweenParens (Strict.Vector.fromList <$> sepByComma Constraint.parse) <* token "=>")
              <*> Type.parse,
            constrain <$> try (Strict.Vector.singleton <$> Constraint.parse <* token "=>") <*> Type.parse,
            mono <$> Type.parse
          ]
   in position
        <**> asum
          [ forallx <**> parseConstraint explicit unqualified,
            parseConstraint implicit mono
          ]
  where
    explicit constraints result parameters startPosition =
      Explicit
        { startPosition,
          parameters,
          constraints,
          result
        }
    unqualified result parameters startPosition =
      Explicit
        { startPosition,
          parameters,
          constraints = Strict.Vector.empty,
          result
        }
    implicit constraints result startPosition = Implicit {startPosition, constraints, result}
    mono result startPosition =
      Implicit
        { startPosition,
          constraints = Strict.Vector.empty,
          result
        }
    forallx = token "forall" *> (Strict.Vector.fromList <$> many TypePattern.parse) <* token "."
