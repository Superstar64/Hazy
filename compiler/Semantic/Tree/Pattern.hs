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
import Semantic.Stage (Stage)
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
  | Constructor
      { constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern stage scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Record
      { constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field stage scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Integer
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        integer :: !Integer,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Float
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        float :: !Rational,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Character
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        string :: !StringLiteral
      }
  | List
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        items :: !(Strict.Vector1 (Pattern stage scope))
      }
  deriving (Show)

instance Shift0.Functor (Pattern stage) where
  map = Shift.mapDefault

instance Shift.Functor (Pattern stage) where
  map category = \case
    Wildcard {names} -> Wildcard {names}
    Constructor {names, irrefutable, constructorPosition, constructor, patterns, single, constructorInfo} ->
      Constructor
        { names,
          irrefutable,
          constructorPosition,
          constructor = Shift.map category constructor,
          patterns = fmap (Shift.map category) patterns,
          single,
          constructorInfo = Shift.map category constructorInfo
        }
    Record {names, irrefutable, constructorPosition, constructor, fields, single, constructorInfo} ->
      Record
        { names,
          irrefutable,
          constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields,
          single,
          constructorInfo = Shift.map category constructorInfo
        }
    Integer {names, irrefutable, startPosition, integer, evidence, equal} ->
      Integer
        { names,
          irrefutable,
          startPosition,
          integer,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Float {names, irrefutable, startPosition, float, evidence, equal} ->
      Float
        { names,
          irrefutable,
          startPosition,
          float,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Character {names, irrefutable, startPosition, character} ->
      Character {names, irrefutable, startPosition, character}
    String {names, irrefutable, startPosition, string} ->
      String
        { names,
          irrefutable,
          startPosition,
          string
        }
    List {names, irrefutable, startPosition, items} ->
      List
        { names,
          irrefutable,
          startPosition,
          items = fmap (Shift.map category) items
        }

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
  Constructor {names, constructorPosition, constructor, patterns, single, constructorInfo} ->
    Constructor {names, irrefutable, constructorPosition, constructor, patterns, single, constructorInfo}
  Record {names, constructorPosition, constructor, fields, single, constructorInfo} ->
    Record {names, irrefutable, constructorPosition, constructor, fields, single, constructorInfo}
  Integer {names, startPosition, integer, evidence, equal} ->
    Integer {names, irrefutable, startPosition, integer, evidence, equal}
  Float {names, startPosition, float, evidence, equal} ->
    Float {names, irrefutable, startPosition, float, evidence, equal}
  Character {names, startPosition, character} -> Character {names, irrefutable, startPosition, character}
  String {names, startPosition, string} -> String {names, irrefutable, startPosition, string}
  List {names, startPosition, items} -> List {names, irrefutable, startPosition, items}
  where
    irrefutable = Prelude.True

type NeverFails :: (Stage -> Environment -> Type) -> Constraint
class NeverFails patternx where
  neverFails :: patternx stage scope -> Prelude.Bool

instance NeverFails Pattern where
  neverFails = \case
    Wildcard {} -> Prelude.True
    patternx | irrefutable patternx -> Prelude.True
    Constructor {single, patterns} -> single && all neverFails patterns
    Record {single, fields} -> single && all neverFails fields
    Integer {} -> Prelude.False
    Float {} -> Prelude.False
    Character {} -> Prelude.False
    String {} -> Prelude.False
    List {} -> Prelude.False

instance NeverFails Field where
  neverFails Field {field} = neverFails field

variable :: Position -> Variable -> Pattern stage scope
variable position name = Wildcard {names = Map.singleton name position}
