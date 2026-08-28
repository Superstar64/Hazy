module Semantic.Tree.TypeDefinition where

import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Unsupported)
import Semantic.Tree.Constraint (Constraint)
import Semantic.Tree.Constructor (Constructor)
import Semantic.Tree.GADTConstructor (GADTConstructor)
import Semantic.Tree.Method (Method)
import Semantic.Tree.Selector (Selector)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

data TypeDefinition stage scope
  = ADT
      { position :: !Position,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position stage scope)),
        constructors :: !(Strict.Vector (Constructor stage (Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | GADT
      { position :: !Position,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position stage scope)),
        gadtConstructors :: !(Strict.Vector (GADTConstructor stage scope)),
        unsupported :: !(Unsupported stage)
      }
  | Class
      { position :: !Position,
        parameter :: !(TypePattern Position stage scope),
        constraints :: !(Strict.Vector (Constraint Position stage scope)),
        methods :: !(Strict.Vector (Method stage (Local ':+ scope)))
      }
  deriving (Show)

instance Shift0.Functor (TypeDefinition stage) where
  map = Shift.mapDefault

instance Shift.Functor (TypeDefinition stage) where
  map category = \case
    ADT {position, brand, parameters, constructors, selectors} ->
      ADT
        { position,
          brand,
          parameters = Shift.map category <$> parameters,
          constructors = fmap (Shift.map (Shift.Over category)) constructors,
          selectors
        }
    GADT {position, brand, parameters, gadtConstructors, unsupported} ->
      GADT
        { position,
          brand,
          parameters = Shift.map category <$> parameters,
          gadtConstructors = fmap (Shift.map category) gadtConstructors,
          unsupported
        }
    Class {position, parameter, methods, constraints} ->
      Class
        { position,
          parameter = Shift.map category parameter,
          constraints = fmap (Shift.map category) constraints,
          methods = fmap (Shift.map (Shift.Over category)) methods
        }

instance FreeTypeVariables TypeDefinition where
  freeTypeVariables target = \case
    ADT {constructors} ->
      concat
        [ foldMap (freeTypeVariables $ FreeVariables.Over target) constructors
        ]
    GADT {gadtConstructors} ->
      concat
        [ foldMap (freeTypeVariables target) gadtConstructors
        ]
    Class {methods, constraints} ->
      concat
        [ foldMap (freeTypeVariables $ FreeVariables.Over target) methods,
          foldMap (freeTypeVariables target) constraints
        ]
