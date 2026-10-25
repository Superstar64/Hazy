{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.Scheme where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser (Parser, asum, betweenParens, many, optional, position, sepByComma, token, try, (<**>))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraint as Constraint
import Syntax.Tree.Constraints (Constraints (..))
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
      constraints = Constraints.Lazy,
      result
    }

parse :: Parser (Scheme Position)
parse =
  parseWith scheme (optional forallx) Nothing Type.parse Type.parse3
  where
    scheme startPosition (Just parameters) constraints result =
      Explicit
        { startPosition,
          parameters,
          constraints,
          result
        }
    scheme startPosition Nothing constraints result =
      Implicit
        { startPosition,
          constraints,
          result
        }
    forallx = token "forall" *> (Strict.Vector.fromList <$> many TypePattern.parse) <* token "."

parseWith ::
  (Position -> parameters -> Constraints Position -> typex -> scheme) ->
  Parser parameters ->
  parameters ->
  Parser typex ->
  Parser typex ->
  Parser scheme
parseWith scheme parameters empty typex typex0 =
  asum
    [ let constraints = Constraints <$> betweenParens elements <* token "=>"
            where
              elements = Strict.Vector.fromList <$> sepByComma Constraint.parse
       in try (scheme <$> position <*> parameters <*> constraints) <*> typex,
      let constraint = Constraints <$> element <* token "=>"
            where
              element = Strict.Vector.singleton <$> Constraint.parse
       in try (scheme <$> position <*> parameters <*> constraint) <*> typex,
      try $ scheme <$> position <*> parameters <*> pure Lazy <*> typex,
      let make parameters typex position = scheme position parameters Strict typex
          body =
            asum
              [ betweenParens $ make <$> parameters <*> typex,
                make empty <$> typex0
              ]
       in position <**> (token "!" *> body),
      let make parameters typex levity position = scheme position parameters Value {levity} typex
          body =
            asum
              [ betweenParens $ make <$> parameters <*> typex,
                make empty <$> typex0
              ]
       in position <**> (token "~!" *> (Type.parse3 <**> body))
    ]
