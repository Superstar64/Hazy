{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Pattern where

import qualified Core.Tree.Evidence as Simple
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Resolve, Stage)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Syntax.Position (Position)
import Syntax.StringLiteral (StringLiteral)
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)
import qualified Prelude

data Pattern stage scope
  = Wildcard
      { names :: !(Map Variable Position)
      }
  | Match
      { names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        match :: !(Bindings stage scope)
      }
  deriving (Show)

instance Shift0.Functor (Pattern stage) where
  map = Shift.mapDefault

instance Shift.Functor (Pattern stage) where
  map category = \case
    Wildcard {names} -> Wildcard {names}
    Match {names, irrefutable, match} ->
      Match
        { names,
          irrefutable,
          match = Shift.map category match
        }

data Bindings stage scope
  = Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern stage scope)),
        failable :: !(Failable stage),
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field stage scope)),
        failable :: !(Failable stage),
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !StringLiteral
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector1 (Pattern stage scope))
      }
  deriving (Show)

instance Shift0.Functor (Bindings stage) where
  map = Shift.mapDefault

instance Shift.Functor (Bindings stage) where
  map category = \case
    Constructor {constructorPosition, constructor, patterns, failable, constructorInfo} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor,
          patterns = fmap (Shift.map category) patterns,
          failable,
          constructorInfo = Shift.map category constructorInfo
        }
    Record {constructorPosition, constructor, fields, failable, constructorInfo} ->
      Record
        { constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields,
          failable,
          constructorInfo = Shift.map category constructorInfo
        }
    Integer {startPosition, integer, evidence, equal} ->
      Integer
        { startPosition,
          integer,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Float {startPosition, float, evidence, equal} ->
      Float
        { startPosition,
          float,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Character {startPosition, character} ->
      Character {startPosition, character}
    String {startPosition, string} ->
      String
        { startPosition,
          string
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = fmap (Shift.map category) items
        }

data Failable stage where
  Exhaustive :: Failable Resolve
  Failable :: Failable Resolve
  Unused :: Failable Check

instance Show (Failable stage) where
  showsPrec _ = \case
    Exhaustive -> showString "Exhaustive"
    Failable -> showString "Failable"
    Unused -> showString "Unused"

data Field stage scope = Field
  { index :: !Int,
    field :: !(Pattern stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Field stage) where
  map = Shift.mapDefault

instance Shift.Functor (Field stage) where
  map category = \case
    Field {index, field} -> Field {index, field = Shift.map category field}

lazy :: Pattern stage scope -> Pattern stage scope
lazy = \case
  Wildcard {names} -> Wildcard {names}
  Match {names, match} -> Match {names, irrefutable = Prelude.True, match}

type NeverFails :: (Stage -> Environment -> Type) -> Constraint
class NeverFails patternx where
  neverFails :: patternx Resolve scope -> Prelude.Bool

instance NeverFails Pattern where
  neverFails = \case
    Wildcard {} -> Prelude.True
    Match {irrefutable, match}
      | irrefutable -> Prelude.True
      | otherwise -> neverFails match

instance NeverFails Bindings where
  neverFails = \case
    Constructor {failable, patterns} -> case failable of
      Exhaustive -> all neverFails patterns
      Failable -> Prelude.False
    Record {failable, fields} -> case failable of
      Exhaustive -> all neverFails fields
      Failable -> Prelude.False
    Integer {} -> Prelude.False
    Float {} -> Prelude.False
    Character {} -> Prelude.False
    String {} -> Prelude.False
    List {} -> Prelude.False

instance NeverFails Field where
  neverFails Field {field} = neverFails field

variable :: Position -> Variable -> Pattern stage scope
variable position name = Wildcard {names = Map.singleton name position}
