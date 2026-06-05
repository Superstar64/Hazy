{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.ClassDeclaration where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict
import Syntax.FreeVariables (TermBindingVariables (termBindingVariables))
import Syntax.Parser (Parser, asum, position, sepBy1Comma, token, try)
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)
import qualified Syntax.Tree.Fixity as Fixity
import Syntax.Tree.LeftHandSide (LeftHandSide)
import qualified Syntax.Tree.LeftHandSide as LeftHandSide
import Syntax.Tree.Marked (Marked ((:@)))
import qualified Syntax.Tree.Marked as Marked
import Syntax.Tree.RightHandSide (RightHandSide)
import qualified Syntax.Tree.RightHandSide as RightHandSide
import Syntax.Tree.Scheme (Scheme)
import qualified Syntax.Tree.Scheme as Scheme
import Syntax.Variable (Name (Variable))

data ClassDeclaration position
  = -- |
    --  > x :: a
    Annotation
      { termNames :: !(Strict.Vector1 (Marked.Variable position)),
        annotation :: !(Scheme position)
      }
  | -- |
    --  > x = e
    Definition
      { startPosition :: !position,
        leftHandSide :: !(LeftHandSide position),
        rightHandSide :: !(RightHandSide position)
      }
  | -- |
    --  > infix `x`
    Infix
      { fixity :: !Fixity,
        termNames' :: !(Strict.Vector1 (Marked.Name position))
      }
  deriving (Show)

instance TermBindingVariables ClassDeclaration where
  termBindingVariables = \case
    Annotation {termNames} -> toList termNames
    Infix {termNames'} -> [position :@ name | position :@ Variable name <- toList termNames']
    Definition {leftHandSide} -> termBindingVariables leftHandSide

parse :: Parser (ClassDeclaration Position)
parse =
  asum
    [ infixx <$> Fixity.parse <*> (Strict.fromNonEmpty <$> sepBy1Comma Marked.parseOperator),
      annotation <$> try (Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral <* token "::") <*> Scheme.parse,
      definition <$> position <*> LeftHandSide.parse <*> RightHandSide.parse (token "=")
    ]
  where
    definition startPosition leftHandSide rightHandSide =
      Definition
        { startPosition,
          leftHandSide,
          rightHandSide
        }
    annotation termNames annotation =
      Annotation
        { termNames,
          annotation
        }
    infixx fixity termNames' =
      Infix
        { fixity,
          termNames'
        }
