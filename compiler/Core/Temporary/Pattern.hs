module Core.Temporary.Pattern where

import Core.Tree.Evidence (Evidence)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Simple.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Semantic
import qualified Semantic.Tree.PatternField as Semantic.Field
import Syntax.StringLiteral (StringLiteral)

data Pattern scope
  = Wildcard
  | Match
      { match :: !(Bindings scope),
        irrefutable :: !Prelude.Bool
      }
  deriving (Show)

data Bindings scope
  = Constructor
      { constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern scope)),
        constructorInfo :: !(ConstructorInfo scope)
      }
  | Record
      { constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope)),
        constructorInfo :: !(ConstructorInfo scope)
      }
  | Integer
      { integer :: !Integer,
        evidence :: !(Evidence scope),
        equal :: !(Evidence scope)
      }
  | Float
      { float :: !Rational,
        evidence :: !(Evidence scope),
        equal :: !(Evidence scope)
      }
  | List {items :: !(Strict.Vector1 (Pattern scope))}
  | Character {character :: !Char}
  | String {string :: !StringLiteral}
  deriving (Show)

instance Shift0.Functor Pattern where
  map = Shift.mapDefault

instance Shift.Functor Pattern where
  map category = \case
    Wildcard -> Wildcard
    Match {match, irrefutable} ->
      Match
        { match = Shift.map category match,
          irrefutable
        }

instance Shift0.Functor Bindings where
  map = Shift.mapDefault

instance Shift.Functor Bindings where
  map category = \case
    Constructor {constructor, patterns, constructorInfo} ->
      Constructor
        { constructor = Shift.map category constructor,
          patterns = Shift.map category <$> patterns,
          constructorInfo = Shift.map category constructorInfo
        }
    Record {constructor, fields, constructorInfo} ->
      Record
        { constructor = Shift.map category constructor,
          fields = Shift.map category <$> fields,
          constructorInfo = Shift.map category constructorInfo
        }
    List {items} ->
      List
        { items = Shift.map category <$> items
        }
    Integer {integer, evidence, equal} ->
      Integer
        { integer,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Float {float, evidence, equal} ->
      Float
        { float,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Character {character} -> Character {character}
    String {string} -> String {string}

instance Shift0.Functor Field where
  map = Shift.mapDefault

instance Shift.Functor Field where
  map category (Field index patternx) = Field index (Shift.map category patternx)

data Field scope = Field !Int !(Pattern scope)
  deriving (Show)

simplify :: Semantic.Pattern Check scope -> Pattern scope
simplify = \case
  Semantic.Wildcard {} -> Wildcard
  patternx ->
    Match
      { match = case patternx of
          Semantic.Constructor {constructor, patterns, constructorInfo = Solved constructorInfo} ->
            Constructor
              { constructor,
                patterns = simplify <$> patterns,
                constructorInfo
              }
          Semantic.Record {constructor, fields, constructorInfo = Solved constructorInfo} ->
            Record
              { constructor,
                fields = resolveField <$> fields,
                constructorInfo
              }
          Semantic.List {items} ->
            List
              { items = simplify <$> items
              }
          Semantic.Integer {integer, evidence = Solved evidence, equal = Solved equal} ->
            Integer
              { integer,
                evidence,
                equal
              }
          Semantic.Float {float, evidence = Solved evidence, equal = Solved equal} ->
            Float
              { float,
                evidence,
                equal
              }
          Semantic.Character {character} -> Character {character}
          Semantic.String {string} -> String {string},
        irrefutable = Semantic.irrefutable patternx
      }

resolveField :: Semantic.Field.Field Check scope -> Field scope
resolveField (Semantic.Field.Field index patternx) = Field index (simplify patternx)
