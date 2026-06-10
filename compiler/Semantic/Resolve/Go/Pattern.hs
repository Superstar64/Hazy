{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Pattern where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Vector1 as Strict.Vector1
import qualified Data.Strict.Vector2 as Strict.Vector2
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateVariableEntries)
import qualified Semantic.Index.Constructor as Constructor (cons, nil, tuple)
import qualified Semantic.Index.Term as Term (Bound (..), Index (..))
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Semantic.Resolve.Binding.Term as Term (Binding (..), Selector (..))
import Semantic.Resolve.Bindings (constructors, stability, terms, types, (</>))
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Context
  ( Context (..),
    (!=),
    (!=~),
  )
import qualified Semantic.Resolve.Go.PatternField as Field (resolve)
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.PatternInfix as Infix (fix, resolve)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Pattern)
import Semantic.Shift (shift)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Pattern (Pattern (..), lazy, variable)
import Semantic.Tree.PatternField (Field (..))
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Pattern as Syntax (Pattern (..))
import qualified Syntax.Tree.PatternInfix as Syntax.Infix
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)
import qualified Prelude

augment :: Pattern Resolve scope' -> Context scope -> Context (Scope.Pattern ':+ scope)
augment patternx context
  | context@Context {locals} <- shift context =
      context
        { locals = bindings patternx </> locals
        }

bindings :: (Monoid stability) => Pattern Resolve scope' -> Bindings.Bindings stability (Scope.Pattern ':+ scope)
bindings patternx =
  Bindings.Bindings
    { terms = Map.map termIndex (selections patternx),
      constructors = Map.empty,
      types = Map.empty,
      stability = mempty
    }
  where
    termIndex (position :@ x) =
      Term.Binding
        { position,
          index = Term2.index $ Term.Pattern x,
          fixity = Fixity {associativity = Left, precedence = 9},
          selector = Term.Normal
        }

selections :: Pattern Resolve scope -> Map Variable (Marked Term.Bound Position)
selections patternx = Map.map single (selections patternx)
  where
    single = \case
      [] -> error "missing bound"
      [(position, patternx)] -> position :@ patternx
      patterns -> duplicateVariableEntries (map fst patterns)

    selections :: Pattern Resolve scope -> Map Variable [(Position, Term.Bound)]
    selections patternx = Map.unionWith (++) outer inner
      where
        outer = Map.map (\position -> [(position, Term.At)]) (names patternx)
        select :: [Pattern Resolve scope] -> Map Variable [(Position, Term.Bound)]
        select patterns = foldr (Map.unionWith (++)) Map.empty $ zipWith pick patterns [0 ..]
          where
            pick :: Pattern Resolve scope -> Int -> Map Variable [(Position, Term.Bound)]
            pick patternx i = (Map.map . map) (\(position, x) -> (position, Term.Select i x)) (selections patternx)
        inner = case patternx of
          Wildcard {} -> select []
          Constructor {patterns} -> select (toList patterns)
          Record {fields} -> select [patternx | Field _ patternx <- toList fields]
          Integer {} -> select []
          Float {} -> select []
          Character {} -> select []
          String {} -> select []
          List {items} -> select (toList items)

resolve :: Context scope -> Syntax.Pattern Position -> Pattern Resolve scope
resolve context = \case
  Syntax.Variable {variable = position :@ name} -> variable position name
  Syntax.At {variable = position :@ variable, patternx} ->
    let pre = resolve context patternx
     in pre {names = Map.insert variable position (names pre)}
  Syntax.Wildcard {} -> Wildcard {names = Map.empty}
  Syntax.Infix {startPosition, left, operator, operatorPosition, right} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.Infix
          { startPosition,
            left,
            operator,
            operatorPosition,
            right
          }
  Syntax.InfixCons {startPosition, left, operatorPosition, right} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.InfixCons
          { startPosition,
            left,
            operatorPosition,
            right
          }
  Syntax.Irrefutable {patternx} -> lazy (resolve context patternx)
  Syntax.Constructor {startPosition, constructor, patterns} ->
    case context !=~ startPosition :@ constructor of
      Constructor.Binding {index = constructor, single} ->
        Constructor
          { names = Map.empty,
            irrefutable = Prelude.False,
            constructorPosition = startPosition,
            constructor,
            patterns = fmap (resolve context) patterns,
            single,
            constructorInfo = Inferred
          }
  Syntax.Record {startPosition, constructor, fields} ->
    case context != startPosition :@ constructor of
      binding@Constructor.Binding
        { index = constructor,
          single
        } ->
          Record
            { names = Map.empty,
              irrefutable = Prelude.False,
              constructorPosition = startPosition,
              constructor,
              fields = fmap (Field.resolve context binding) fields,
              single,
              constructorInfo = Inferred
            }
  Syntax.Integer {startPosition, integer} ->
    Integer
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        integer,
        evidence = Inferred,
        equal = Inferred
      }
  Syntax.Float {startPosition, float} ->
    Float
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        float,
        evidence = Inferred,
        equal = Inferred
      }
  Syntax.NegativeInteger {startPosition, integer} ->
    Integer
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        integer = -integer,
        evidence = Inferred,
        equal = Inferred
      }
  Syntax.NegativeFloat {startPosition, float} ->
    Float
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        float = -float,
        evidence = Inferred,
        equal = Inferred
      }
  Syntax.Character {startPosition, character} ->
    Character
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        character
      }
  Syntax.String {startPosition, string} ->
    String
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        string
      }
  Syntax.Unit {startPosition} ->
    Constructor
      { names = Map.empty,
        irrefutable = Prelude.False,
        constructorPosition = startPosition,
        constructor = Constructor.tuple 0,
        patterns = Strict.Vector.empty,
        single = Prelude.True,
        constructorInfo = Inferred
      }
  Syntax.Tuple {startPosition, elements} ->
    Constructor
      { names = Map.empty,
        irrefutable = Prelude.False,
        constructorPosition = startPosition,
        constructor = Constructor.tuple (length elements),
        patterns = resolve context <$> Strict.Vector2.toVector elements,
        single = Prelude.True,
        constructorInfo = Inferred
      }
  Syntax.List {startPosition, items}
    | null items ->
        Constructor
          { names = Map.empty,
            irrefutable = Prelude.False,
            constructorPosition = startPosition,
            constructor = Constructor.nil,
            patterns = Strict.Vector.empty,
            single = Prelude.False,
            constructorInfo = Inferred
          }
    | otherwise ->
        List
          { names = Map.empty,
            irrefutable = Prelude.False,
            startPosition,
            items = fmap (resolve context) (Strict.Vector1.fromVector items)
          }
  Syntax.Cons {startPosition, head, tail} ->
    Constructor
      { names = Map.empty,
        irrefutable = Prelude.False,
        constructorPosition = startPosition,
        constructor = Constructor.cons,
        patterns = Strict.Vector.fromList [resolve context head, resolve context tail],
        single = Prelude.False,
        constructorInfo = Inferred
      }
