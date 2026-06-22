{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for module exports
module Syntax.Tree.Exports where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser
  ( Parser,
    asum,
    betweenParens,
    sepEndByComma,
  )
import Syntax.Tree.ExportSymbol (Symbol)
import qualified Syntax.Tree.ExportSymbol as ExportSymbol

data Exports
  = -- |
    -- > module M ( x )
    -- >          ^^^^^
    Exports {exports :: !(Strict.Vector Symbol)}
  | Default
  deriving (Show)

parse :: Parser Exports
parse =
  asum
    [ exports . Strict.Vector.fromList <$> betweenParens (sepEndByComma ExportSymbol.parse),
      pure Default
    ]
  where
    exports exports = Exports {exports}
