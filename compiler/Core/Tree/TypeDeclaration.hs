module Core.Tree.TypeDeclaration where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Class (Class)
import Core.Tree.Data (Data)
import Core.Tree.TypeDefinition (TypeDefinition)
import qualified Core.Tree.TypeDefinition as TypeDefinition
import qualified Semantic.Check.Go.TypeDeclaration as Solved (TypeDeclaration (..))
import Semantic.Layout (Normal)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.TypeDefinition2 (TypeDefinition2 (..))
import Syntax.Lexer (ConstructorIdentifier)

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

simplify :: Solved.TypeDeclaration locality Normal Check scope -> TypeDeclaration scope
simplify Solved.TypeDeclaration {name, definition} =
  TypeDeclaration
    { name,
      definition = case definition of
        _ ::: definition -> TypeDefinition.simplify definition
    }
