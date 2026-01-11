{-# LANGUAGE_HAZY UnorderedRecords #-}
module Stage1.Tree.ClassDeclarations where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser (Parser, asum, betweenBraces, sepEndBySemicolon, token)
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.ClassDeclaration (ClassDeclaration)
import qualified Stage1.Tree.ClassDeclaration as ClassDeclaration

newtype ClassDeclarations position
  = -- |
    -- > Class A a where { x :: a }
    -- >           ^^^^^^^^^^^^^^^^
    ClassDeclarations
    { declarations :: Strict.Vector (ClassDeclaration position)
    }
  deriving (Show)

instance TermBindingVariables ClassDeclarations where
  termBindingVariables ClassDeclarations {declarations} =
    foldMap termBindingVariables declarations

parse :: Parser (ClassDeclarations Position)
parse =
  asum
    [ token "where" *> parse,
      pure $ classDeclarations Strict.Vector.empty
    ]
  where
    classDeclarations declarations = ClassDeclarations {declarations}
    parse :: Parser (ClassDeclarations Position)
    parse =
      classDeclarations . Strict.Vector.fromList
        <$> betweenBraces (sepEndBySemicolon ClassDeclaration.parse)
