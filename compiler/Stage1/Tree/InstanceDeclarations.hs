{-# LANGUAGE_HAZY UnorderedRecords #-}
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
    InstanceDeclarations
    { declarations :: Strict.Vector (InstanceDeclaration position)
    }
  deriving (Show)

parse :: Parser (InstanceDeclarations Position)
parse =
  asum
    [ token "where" *> parse,
      pure (instanceDeclarations Strict.Vector.empty)
    ]
  where
    instanceDeclarations declarations = InstanceDeclarations {declarations}
    parse :: Parser (InstanceDeclarations Position)
    parse =
      instanceDeclarations . Strict.Vector.fromList
        <$> betweenBraces (sepEndBySemicolon InstanceDeclaration.parse)
