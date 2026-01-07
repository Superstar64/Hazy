module Stage1.Tree.InstanceDeclarations where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser (Parser, asum, betweenBraces, sepEndBySemicolon, token)
import Stage1.Position (Position)
import Stage1.Tree.InstanceDeclaration (InstanceDeclaration)
import qualified Stage1.Tree.InstanceDeclaration as InstanceDeclaration

newtype InstanceDeclarations position
  = -- |
    -- > Instance C A where { x = A }
    -- >              ^^^^^^^^^^^^^^^
    InstanceDeclarations (Strict.Vector (InstanceDeclaration position))
  deriving (Show)

parse :: Parser (InstanceDeclarations Position)
parse =
  asum
    [ token "where" *> parse,
      pure (InstanceDeclarations Strict.Vector.empty)
    ]
  where
    parse :: Parser (InstanceDeclarations Position)
    parse =
      InstanceDeclarations . Strict.Vector.fromList
        <$> betweenBraces (sepEndBySemicolon InstanceDeclaration.parse)
