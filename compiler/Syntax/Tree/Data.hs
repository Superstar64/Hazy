{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for data types
module Syntax.Tree.Data where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    asum,
    betweenBraces,
    betweenParens,
    sepByComma,
    sepEndBySemicolon,
    token,
  )
import Syntax.Position (Position)
import Syntax.Tree.Constructor (Constructor)
import qualified Syntax.Tree.Constructor as Constructor
import Syntax.Tree.GADTConstructor (GADTConstructor)
import qualified Syntax.Tree.GADTConstructor as GADTConstructor
import Syntax.Variable (QualifiedConstructorIdentifier)
import qualified Syntax.Variable as Variable

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
