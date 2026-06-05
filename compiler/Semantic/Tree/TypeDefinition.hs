module Semantic.Tree.TypeDefinition where

import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Unsupported)
import Semantic.Tree.Constraint (Constraint)
import Semantic.Tree.Constructor (Constructor)
import Semantic.Tree.GADTConstructor (GADTConstructor)
import Semantic.Tree.Method (Method)
import Semantic.Tree.Selector (Selector)
import Semantic.Tree.Type (Type)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

data Equality
  = Constructive
  | Substitutive

type Constructive = 'Constructive

type Substitutive = 'Substitutive

data Inject equality where
  Inject :: Inject Constructive

instance Show (Inject equality) where
  show Inject = "Inject"

data Alias equality where
  Alias :: Alias Substitutive

instance Show (Alias equality) where
  show Alias = "Alias"

data TypeDefinition equality stage scope
  = ADT
      { position :: !Position,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position stage scope)),
        constructors :: !(Strict.Vector (Constructor stage (Local ':+ scope))),
        selectors :: !(Strict.Vector Selector),
        inject :: !(Inject equality)
      }
  | GADT
      { position :: !Position,
        brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern Position stage scope)),
        gadtConstructors :: !(Strict.Vector (GADTConstructor stage scope)),
        unsupported :: !(Unsupported stage),
        inject :: !(Inject equality)
      }
  | Class
      { position :: !Position,
        parameter :: !(TypePattern Position stage scope),
        constraints :: !(Strict.Vector (Constraint Position stage scope)),
        methods :: !(Strict.Vector (Method stage (Local ':+ scope))),
        inject :: !(Inject equality)
      }
  | Synonym
      { parameters :: !(Strict.Vector (TypePattern Position stage scope)),
        synonym :: !(Type Position stage (Local ':+ scope)),
        alias :: !(Alias equality)
      }
  deriving (Show)

instance Shift (TypeDefinition equality stage) where
  shift = shiftDefault

instance Shift.Functor (TypeDefinition equality stage) where
  map category = \case
    ADT {position, brand, parameters, constructors, selectors, inject} ->
      ADT
        { position,
          brand,
          parameters = Shift.map category <$> parameters,
          constructors = fmap (Shift.map (Shift.Over category)) constructors,
          selectors,
          inject
        }
    GADT {position, brand, parameters, gadtConstructors, unsupported, inject} ->
      GADT
        { position,
          brand,
          parameters = Shift.map category <$> parameters,
          gadtConstructors = fmap (Shift.map category) gadtConstructors,
          unsupported,
          inject
        }
    Class {position, parameter, methods, constraints, inject} ->
      Class
        { position,
          parameter = Shift.map category parameter,
          constraints = fmap (Shift.map category) constraints,
          methods = fmap (Shift.map (Shift.Over category)) methods,
          inject
        }
    Synonym {parameters, synonym, alias} ->
      Synonym
        { parameters = Shift.map category <$> parameters,
          synonym = Shift.map (Shift.Over category) synonym,
          alias
        }

instance FreeTypeVariables (TypeDefinition equality) where
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

assumeInject :: TypeDefinition equality stage scope -> Inject equality
assumeInject = \case
  ADT {inject} -> inject
  GADT {inject} -> inject
  Class {inject} -> inject
  Synonym {} -> error "bad assumeInject"
