module Stage1.Tree.ClassDeclaration where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict
import Stage1.Parser (Parser, asum, position, sepBy1Comma, token, try)
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (termBindingVariables))
import Stage1.Tree.Fixity (Fixity)
import qualified Stage1.Tree.Fixity as Fixity
import Stage1.Tree.LeftHandSide (LeftHandSide)
import qualified Stage1.Tree.LeftHandSide as LeftHandSide
import Stage1.Tree.Marked (Marked ((:@)))
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.RightHandSide (RightHandSide)
import qualified Stage1.Tree.RightHandSide as RightHandSide
import Stage1.Tree.Scheme (Scheme)
import qualified Stage1.Tree.Scheme as Scheme
import Stage1.Variable (Name (Variable))

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
