{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for declaration groups
module Stage1.Tree.Declarations where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    betweenBraces,
    sepEndBySemicolon,
  )
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Declaration (Declaration)
import qualified Stage1.Tree.Declaration as Declaration

newtype Declarations position
  = -- |
    -- > let { x = e ; y = e } in e
    -- >     ^^^^^^^^^^^^^^^^^
    Declarations {declarations :: Strict.Vector (Declaration position)}
  deriving (Show)

instance TermBindingVariables Declarations where
  termBindingVariables Declarations {declarations} = foldMap termBindingVariables declarations

parse :: Parser (Declarations Position)
parse =
  declarations . Strict.Vector.fromList
    <$> betweenBraces (sepEndBySemicolon Declaration.parse)
  where
    declarations declarations = Declarations {declarations}

empty = Declarations {declarations = Strict.Vector.empty}
