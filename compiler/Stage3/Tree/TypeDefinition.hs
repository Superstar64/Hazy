module Stage3.Tree.TypeDefinition where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check)
import Stage2.Tree.Constraint (Constraint)
import Stage2.Tree.Constructor (Constructor)
import Stage2.Tree.Method (Method)
import Stage2.Tree.Selector (Selector)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypePattern (TypePattern)

data TypeDefinition scope
  = ADT
      { brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position Check scope)),
        constructors :: !(Strict.Vector (Constructor Check (Scope.Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | Class
      { parameter :: !(TypePattern Position Check scope),
        constraints :: !(Strict.Vector (Constraint Position Check scope)),
        methods :: !(Strict.Vector (Method Check (Scope.Local ':+ scope)))
      }
  | Synonym
      { parameters :: !(Strict.Vector (TypePattern Position Check scope)),
        synonym :: !(Type Position Check (Scope.Local ':+ scope))
      }
  deriving (Show)
