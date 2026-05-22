module Stage4.Tree.TypeDeclaration where

import Stage1.Lexer (ConstructorIdentifier)
import Stage2.Layout (Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.TypeDefinition2 (TypeDefinition2 (..))
import qualified Stage3.Tree.TypeDeclaration as Solved (TypeDeclaration (..))
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Class (Class)
import Stage4.Tree.Data (Data)
import Stage4.Tree.TypeDefinition (TypeDefinition)
import qualified Stage4.Tree.TypeDefinition as TypeDefinition

data TypeDeclaration scope
  = TypeDeclaration
  { name :: !ConstructorIdentifier,
    definition :: TypeDefinition scope
  }
  deriving (Show)

assumeData :: TypeDeclaration scope -> Data scope
assumeData TypeDeclaration {definition} = TypeDefinition.assumeData definition

assumeClass :: TypeDeclaration scope -> Class scope
assumeClass TypeDeclaration {definition} = TypeDefinition.assumeClass definition

instance Shift TypeDeclaration where
  shift = shiftDefault

instance Shift.Functor TypeDeclaration where
  map = Shift2.mapDefault

instance Shift2.Functor TypeDeclaration where
  map = Substitute.mapDefault

instance Substitute.Functor TypeDeclaration where
  map category = \case
    TypeDeclaration {name, definition} ->
      TypeDeclaration
        { name,
          definition = Substitute.map category definition
        }

simplify :: Solved.TypeDeclaration locality Normal scope -> TypeDeclaration scope
simplify Solved.TypeDeclaration {name, definition} =
  TypeDeclaration
    { name,
      definition = case definition of
        _ ::: definition -> TypeDefinition.simplify definition
    }
