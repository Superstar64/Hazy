module Core.Tree.TypeDeclaration where

import qualified Core.Substitute as Substitute
import Core.Tree.Class (Class)
import Core.Tree.Data (Data)
import Core.Tree.TypeDefinition (TypeDefinition)
import qualified Core.Tree.TypeDefinition as TypeDefinition
import Data.Functor.Identity (Identity)
import Semantic.Layout (Normal)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.TypeDeclaration as Solved (TypeDeclaration (..))
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

instance Shift0.Functor TypeDeclaration where
  map = Shift.mapDefault

instance Shift.Functor TypeDeclaration where
  map = Substitute.mapDefault

instance Substitute.Functor TypeDeclaration where
  map category = \case
    TypeDeclaration {name, definition} ->
      TypeDeclaration
        { name,
          definition = Substitute.map category definition
        }

simplify :: Solved.TypeDeclaration locality Identity Normal Check scope -> TypeDeclaration scope
simplify Solved.TypeDeclaration {name, definition} =
  TypeDeclaration
    { name,
      definition = TypeDefinition.simplify definition
    }
