module Syntax.Tree.Constraints where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.FreeVariables (FreeTypeVariables (..))
import Syntax.Parser (Parser, asum, betweenParens, sepByComma, token, try)
import Syntax.Position (Position)
import Syntax.Tree.Constraint (Constraint)
import qualified Syntax.Tree.Constraint as Constraint

data Constraints position
  = Constraints !(Strict.Vector (Constraint position))
  | None
  deriving (Show)

instance FreeTypeVariables Constraints where
  freeTypeVariables = \case
    Constraints constraints -> foldMap freeTypeVariables constraints
    None -> []

parse :: Parser (Constraints Position)
parse =
  asum
    [ try $ Constraints <$> betweenParens (Strict.Vector.fromList <$> sepByComma Constraint.parse) <* token "=>",
      try $ Constraints <$> (Strict.Vector.singleton <$> Constraint.parse) <* token "=>",
      pure $ None
    ]
