{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.InstanceDeclarations where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser (Parser, asum, betweenBraces, sepEndBySemicolon, token)
import Syntax.Position (Position)
import Syntax.Tree.InstanceDeclaration (InstanceDeclaration)
import qualified Syntax.Tree.InstanceDeclaration as InstanceDeclaration

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
