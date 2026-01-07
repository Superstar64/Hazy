-- |
-- Parser syntax tree for module exports
module Stage1.Tree.Exports where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenBuiltinPragma,
    betweenParens,
    sepEndByComma,
  )
import Stage1.Tree.ExportSymbol (Symbol)
import qualified Stage1.Tree.ExportSymbol as ExportSymbol

data Exports
  = -- |
    -- > module M ( x )
    -- >          ^^^^^
    Exports !(Strict.Vector Symbol)
  | Builtin
  | Default
  deriving (Show)

parse :: Parser Exports
parse =
  asum
    [ Exports . Strict.Vector.fromList <$> betweenParens (sepEndByComma ExportSymbol.parse),
      Builtin <$ betweenBuiltinPragma (pure ()),
      pure Default
    ]
