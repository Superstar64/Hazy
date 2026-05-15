module Stage3.Tree.TypeDefinition where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check)
import Stage2.Tree.Selector (Selector)
import Stage2.Tree.Type (Type)
import Stage3.Tree.Constraint (Constraint)
import Stage3.Tree.Constructor (Constructor)
import Stage3.Tree.Method (Method)
import Stage3.Tree.TypePattern (TypePattern)
import qualified Stage4.Tree.Type as Simple (Type)

data TypeDefinition scope
  = ADT
      { brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern scope)),
        constructors :: !(Strict.Vector (Constructor (Scope.Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | Class
      { parameter :: !(TypePattern scope),
        constraints :: !(Strict.Vector (Constraint scope)),
        methods :: !(Strict.Vector (Method (Scope.Local ':+ scope)))
      }
  | Synonym
      { definition :: !(Type Position Check (Scope.Local ':+ scope)),
        definition' :: !(Simple.Type (Scope.Local ':+ scope))
      }
  deriving (Show)
