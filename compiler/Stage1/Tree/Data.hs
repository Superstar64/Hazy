-- |
-- Parser syntax tree for data types
module Stage1.Tree.Data where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenBraces,
    betweenParens,
    sepByComma,
    sepEndBySemicolon,
    token,
  )
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Constructor (Constructor)
import qualified Stage1.Tree.Constructor as Constructor
import Stage1.Tree.GADTConstructor (GADTConstructor)
import qualified Stage1.Tree.GADTConstructor as GADTConstructor
import Stage1.Variable (QualifiedConstructorIdentifier)
import qualified Stage1.Variable as Variable

data Data position
  = -- |
    --  > data T = A
    --  >        ^^^
    ADT !(Strict.Vector (Constructor position)) !(Strict.Vector QualifiedConstructorIdentifier)
  | -- |
    --  > data T where { A :: T }
    --  >        ^^^^^^^^^^^^^^^^
    GADT !(Strict.Vector GADTConstructor)
  deriving (Show)

instance TermBindingVariables Data where
  termBindingVariables (ADT constructors _) = foldMap termBindingVariables constructors
  termBindingVariables _ = mempty

parse :: Parser (Data Position)
parse =
  asum
    [ GADT <$ token "where" <*> betweenBraces (Strict.Vector.fromList <$> sepEndBySemicolon GADTConstructor.parse),
      ADT <$> Constructor.parseMany <*> parseDerive
    ]

parseDerive :: Parser (Strict.Vector QualifiedConstructorIdentifier)
parseDerive =
  asum
    [ token "deriving"
        *> asum
          [ betweenParens (Strict.Vector.fromList <$> sepByComma Variable.parse),
            Strict.Vector.singleton <$> Variable.parse
          ],
      pure Strict.Vector.empty
    ]
