{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for left hand sides
module Stage1.Tree.LeftHandSide where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict (Vector1, fromList')
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenParens,
    many,
    try,
  )
import Stage1.ParserCombinator (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.Pattern (Pattern)
import qualified Stage1.Tree.Pattern as Pattern
import Stage1.Tree.PatternInfix (Infix)
import qualified Stage1.Tree.PatternInfix as PatternInfix

data LeftHandSide position
  = -- |
    -- > x = e
    -- > ^
    Pattern !(Pattern position)
  | -- |
    -- > f x = e
    -- > ^^^
    Prefix
      { variable :: !(Marked.Variable position),
        parameters' :: !(Strict.Vector1 (Pattern position))
      }
  | -- |
    -- > x + x = e
    -- > ^^^^^
    Binary
      { leftHandSide :: !(Infix position),
        operator :: !(Marked.Variable position),
        rightHandSide :: !(Infix position),
        parameters :: !(Strict.Vector (Pattern position))
      }
  deriving (Show)

{-# UnorderedRecords Prefix, Binary #-}

instance TermBindingVariables LeftHandSide where
  termBindingVariables = \case
    Pattern pattern -> termBindingVariables pattern
    Prefix {variable} -> [variable]
    Binary {operator} -> [operator]

parse :: Parser (LeftHandSide Position)
parse =
  asum
    [ parse1,
      Pattern <$> Pattern.parse
    ]

parse1 :: Parser (LeftHandSide Position)
parse1 =
  asum
    [ try (prefix <$> Marked.parseLiteral <*> Pattern.parse2) <*> many Pattern.parse2,
      try (binary <$> PatternInfix.parse <*> Marked.parseOperator <*> PatternInfix.parse),
      try (parens <$> betweenParens parse1) <*> many Pattern.parse2
    ]
  where
    prefix variable pattern patterns =
      Prefix
        { variable,
          parameters' = Strict.fromList' pattern patterns
        }
    binary leftHandSide operator rightHandSide =
      Binary
        { leftHandSide,
          operator,
          rightHandSide = rightHandSide,
          parameters = Strict.Vector.empty
        }
    parens function additional = case function of
      Prefix {variable, parameters'} ->
        Prefix
          { variable,
            parameters' = prefixes $ toList parameters' <> additional
          }
        where
          prefixes (prefix : prefixes) = Strict.fromList' prefix prefixes
          prefixes _ = error "empty prefixes"
      Binary {leftHandSide, operator, rightHandSide, parameters} ->
        Binary
          { leftHandSide,
            operator,
            rightHandSide,
            parameters = Strict.Vector.fromList $ toList parameters <> additional
          }
      Pattern {} -> error "unexpected pattern"
