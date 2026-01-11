{-# LANGUAGE_HAZY UnorderedRecords #-}

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
    ADT
      { constructors :: !(Strict.Vector (Constructor position)),
        derivingx :: !(Strict.Vector QualifiedConstructorIdentifier)
      }
  | -- |
    --  > data T where { A :: T }
    --  >        ^^^^^^^^^^^^^^^^
    GADT {gadtConstructors :: !(Strict.Vector GADTConstructor)}
  deriving (Show)

instance TermBindingVariables Data where
  termBindingVariables ADT {constructors} = foldMap termBindingVariables constructors
  termBindingVariables _ = mempty

parse :: Parser (Data Position)
parse =
  asum
    [ gadt
        <$ token "where"
        <*> betweenBraces
          (Strict.Vector.fromList <$> sepEndBySemicolon GADTConstructor.parse),
      adt <$> Constructor.parseMany <*> parseDerive
    ]
  where
    adt constructors derivingx = ADT {constructors, derivingx}
    gadt gadtConstructors = GADT {gadtConstructors}

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
