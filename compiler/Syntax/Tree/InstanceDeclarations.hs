{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.InstanceDeclarations where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser (Parser, asum, betweenBraces, sepEndBySemicolon, token)
import Syntax.Position (Position)
import Syntax.Tree.InstanceDeclaration (InstanceDeclaration)
import qualified Syntax.Tree.InstanceDeclaration as InstanceDeclaration

data InstanceDeclarations position
  = -- |
    -- > instance C A where { x = A }
    -- >              ^^^^^^^^^^^^^^^
    InstanceDeclarations
      { declarations :: !(Strict.Vector (InstanceDeclaration position))
      }
  | -- |
    -- > deriving instance C A
    --                         ^
    DerivingInstance
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

_derivingInstance :: InstanceDeclarations position
_derivingInstance = DerivingInstance
