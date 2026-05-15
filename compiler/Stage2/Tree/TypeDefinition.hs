module Stage2.Tree.TypeDefinition where

import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.Constraint (Constraint)
import Stage2.Tree.Constructor (Constructor)
import Stage2.Tree.GADTConstructor (GADTConstructor)
import Stage2.Tree.Method (Method)
import Stage2.Tree.Selector (Selector)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypePattern (TypePattern)

data TypeDefinition scope
  = ADT
      { position :: !Position,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
        constructors :: !(Strict.Vector (Constructor Resolve (Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | GADT
      { position :: !Position,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
        gadtConstructors :: !(Strict.Vector (GADTConstructor scope))
      }
  | Class
      { position :: !Position,
        parameter :: !(TypePattern Position Resolve scope),
        methods :: !(Strict.Vector (Method (Local ':+ scope))),
        constraints :: !(Strict.Vector (Constraint Position scope))
      }
  | Synonym
      { parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
        synonym :: !(Type Position Resolve (Local ':+ scope))
      }
  deriving (Show)

instance Shift TypeDefinition where
  shift = shiftDefault

instance Shift.Functor TypeDefinition where
  map category = \case
    ADT {position, brand, parameters, constructors, selectors} ->
      ADT
        { position,
          brand,
          parameters = Shift.map category <$> parameters,
          constructors = fmap (Shift.map (Shift.Over category)) constructors,
          selectors
        }
    GADT {position, brand, parameters, gadtConstructors} ->
      GADT
        { position,
          brand,
          parameters = Shift.map category <$> parameters,
          gadtConstructors = fmap (Shift.map category) gadtConstructors
        }
    Class {position, parameter, methods, constraints} ->
      Class
        { position,
          parameter = Shift.map category parameter,
          constraints = fmap (Shift.map category) constraints,
          methods = fmap (Shift.map (Shift.Over category)) methods
        }
    Synonym {parameters, synonym} ->
      Synonym
        { parameters = Shift.map category <$> parameters,
          synonym = Shift.map (Shift.Over category) synonym
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
    Synonym {synonym} ->
      concat
        [ freeTypeVariables (FreeVariables.Over target) synonym
        ]
