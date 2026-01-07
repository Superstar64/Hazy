{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for constraints
module Stage1.Tree.Constraint where

import Control.Applicative ((<**>))
import qualified Control.Applicative as Applicative (some)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.FreeTypeVariables (FreeTypeVariables (..))
import Stage1.Parser
  ( Parser,
    asum,
    betweenParens,
    position,
  )
import Stage1.Position (Position)
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.Type (Type)
import qualified Stage1.Tree.Type as Type

data Constraint position
  = -- |
    --  > x :: F a => a
    --  >      ^^^
    Constraint
    { startPosition :: !position,
      classVariable :: !(Marked.QualifiedConstructorIdentifier position),
      typeVariable :: !(Marked.VariableIdentifier position),
      arguments :: !(Strict.Vector (Type position))
    }
  deriving (Show)

instance FreeTypeVariables Constraint where
  freeTypeVariables Constraint {typeVariable, arguments} = typeVariable : foldMap freeTypeVariables arguments

parse :: Parser (Constraint Position)
parse = Marked.parse <**> body
  where
    body =
      asum
        [ makeSimple <$> position <*> Marked.parse,
          betweenParens $
            makeComplex
              <$> position
              <*> Marked.parse
              <*> (Strict.Vector.fromList <$> Applicative.some Type.parse3)
        ]
    makeSimple startPosition typeVariable classVariable =
      Constraint
        { startPosition,
          classVariable,
          typeVariable,
          arguments = Strict.Vector.empty
        }
    makeComplex startPosition typeVariable arguments classVariable =
      Constraint
        { startPosition,
          classVariable,
          typeVariable,
          arguments
        }
