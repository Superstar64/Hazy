module Semantic.Check.Temporary.Pattern where

import Control.Monad (when)
import Control.Monad.ST (ST)
import qualified Core.Tree.Builtin as Builtin
import Core.Tree.TypeDeclaration (assumeData)
import Data.Map (Map)
import Data.Strict.Vector1 (toVector)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector1 as Strict.Vector1
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( mismatchedConstructorArguments,
  )
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context (..))
import Semantic.Check.DataInstance (DataInstance (DataInstance))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.Simple.Data as Simple.Data
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import Semantic.Check.Temporary.PatternField (Field)
import qualified Semantic.Check.Temporary.PatternField as Field
import Semantic.Check.TermBinding (TermBinding (TermBinding), Type (..))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type
import Semantic.Index.Term (Bound)
import qualified Semantic.Index.Term as Bound (Bound (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..))
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Pattern as Semantic
import qualified Semantic.Tree.Pattern as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern s scope
  = Wildcard
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position)
      }
  | Constructor
      { typex :: !(Unify.Type s scope),
        constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern s scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Record
      { typex :: !(Unify.Type s scope),
        constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field s scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Integer
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Float
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Character
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        startPosition :: !Position,
        irrefutable :: !Prelude.Bool,
        character :: !Char
      }
  | String
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        startPosition :: !Position,
        irrefutable :: !Prelude.Bool,
        string :: !Text
      }
  | List
      { typex :: !(Unify.Type s scope),
        names :: !(Map Variable Position),
        startPosition :: !Position,
        irrefutable :: !Prelude.Bool,
        items :: !(Strict.Vector1 (Pattern s scope))
      }

(!) :: Pattern s scope -> Bound -> Unify.Type s scope
patternx ! Bound.At = typex patternx
patternx ! Bound.Select index bound = case patternx of
  Constructor {patterns} -> patterns Strict.Vector.! index ! bound
  Record {fields} -> fields Strict.Vector.! index Field.! bound
  List {items} -> toVector items Strict.Vector.! index ! bound
  Integer {} -> error "bad index"
  Float {} -> error "bad index"
  Character {} -> error "bad index"
  String {} -> error "bad index"
  Wildcard {} -> error "bad index"

augment :: Pattern s scope -> Context s scope -> Context s (Scope.Pattern ':+ scope)
augment patternx Context {termEnvironment, localEnvironment, typeEnvironment} =
  Context
    { termEnvironment = Term.Pattern (augmentPattern patternx) termEnvironment,
      localEnvironment = Local.Pattern localEnvironment,
      typeEnvironment = Type.Pattern typeEnvironment
    }

augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentPattern patternx = Term.Bound {at, select}
  where
    at = TermBinding $ pure $ Wobbly (shift (Unify.monoScheme $ typex patternx))
    select = case patternx of
      Wildcard {} -> Strict.Vector.empty
      List {items} -> Strict.Vector.map augmentPattern $ Strict.Vector1.toVector items
      Constructor {patterns} -> fmap augmentPattern patterns
      Record {fields} -> fmap Field.augmentField fields
      Integer {} -> Strict.Vector.empty
      Float {} -> Strict.Vector.empty
      Character {} -> Strict.Vector.empty
      String {} -> Strict.Vector.empty

check :: Context s scope -> Unify.Type s scope -> Semantic.Pattern Resolve scope -> ST s (Pattern s scope)
check context@Context {typeEnvironment} typex = \case
  Semantic.Wildcard {names} -> pure Wildcard {typex, names}
  Semantic.Constructor
    { irrefutable,
      constructorPosition,
      constructor,
      patterns,
      names,
      single
    } ->
      do
        let Constructor.Index typeIndex constructorIndex = constructor
        datax <- do
          let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        DataInstance {types, constructors} <-
          Simple.Data.instanciate context constructorPosition datax
        let root = Unify.constructor typeIndex
            base = foldl Unify.call root types
            instancex = constructors Strict.Vector.! constructorIndex
            entries = ConstructorInstance.types instancex
            constructorInfo = ConstructorInstance.info instancex
        Unify.unify context constructorPosition typex base
        when (length entries /= length patterns) $ mismatchedConstructorArguments constructorPosition
        patterns <- sequence $ Strict.Vector.zipWith (check context) entries patterns
        pure $
          Constructor
            { typex,
              constructorPosition,
              names,
              irrefutable,
              constructor,
              patterns,
              single,
              constructorInfo
            }
  Semantic.Record
    { irrefutable,
      constructorPosition,
      constructor,
      fields,
      names,
      single
    } -> do
      let Constructor.Index typeIndex constructorIndex = constructor
      datax <- do
        let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
        Builtin.index pure get typeIndex
      DataInstance {types, constructors} <-
        Simple.Data.instanciate context constructorPosition datax
      let root = Unify.constructor typeIndex
          base = foldl Unify.call root types
          instancex = constructors Strict.Vector.! constructorIndex
          entries = ConstructorInstance.types instancex
          constructorInfo = ConstructorInstance.info instancex
          lookup index = entries Strict.Vector.! index
      Unify.unify context constructorPosition typex base
      fields <- traverse (Field.check context lookup) fields
      pure $
        Record
          { typex,
            names,
            constructorPosition,
            irrefutable,
            constructor,
            fields,
            single,
            constructorInfo
          }
  Semantic.List {irrefutable, names, startPosition, items} -> do
    element <- Unify.fresh Unify.typex
    items <- traverse (check context element) items
    Unify.unify context startPosition typex (Unify.listWith element)
    pure $ List {typex, names, startPosition, irrefutable, items}
  Semantic.Character {irrefutable, names, startPosition, character} -> do
    Unify.unify context startPosition typex Unify.char
    pure $ Character {typex, names, startPosition, irrefutable, character}
  Semantic.String {irrefutable, names, startPosition, string} -> do
    Unify.unify context startPosition typex (Unify.listWith Unify.char)
    pure $ String {typex, names, startPosition, irrefutable, string}
  Semantic.Integer {irrefutable, names, startPosition, integer} -> do
    evidence <- Unify.constrain context startPosition Type2.Num typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Integer {typex, names, irrefutable, startPosition, integer, evidence, equal}
  Semantic.Float {irrefutable, names, startPosition, float} -> do
    evidence <- Unify.constrain context startPosition Type2.Fractional typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Float {typex, names, irrefutable, startPosition, float, evidence, equal}

solve :: Pattern s scope -> Unify.Solve s (Solved.Pattern Check scope)
solve = \case
  Wildcard {names} -> pure Solved.Wildcard {names}
  Constructor
    { irrefutable,
      names,
      single,
      constructorPosition,
      constructor,
      patterns,
      constructorInfo
    } -> do
      patterns <- traverse solve patterns
      constructorInfo <- ConstructorInfo.solve constructorInfo
      pure
        Solved.Constructor
          { irrefutable,
            names,
            constructorPosition,
            constructor,
            patterns,
            single,
            constructorInfo = Solved constructorInfo
          }
  Record
    { irrefutable,
      names,
      constructorPosition,
      constructor,
      fields,
      single,
      constructorInfo
    } -> do
      fields <- traverse Field.solve fields
      constructorInfo <- ConstructorInfo.solve constructorInfo
      pure
        Solved.Record
          { irrefutable,
            names,
            constructorPosition,
            constructor,
            fields,
            single,
            constructorInfo = Solved constructorInfo
          }
  List {irrefutable, startPosition, names, items} -> do
    items <- traverse solve items
    pure Solved.List {irrefutable, startPosition, names, items}
  Integer {irrefutable, names, startPosition, integer, evidence, equal} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    equal <- Unify.solveEvidence startPosition equal
    pure
      Solved.Integer
        { irrefutable,
          names,
          startPosition,
          integer,
          evidence = Solved evidence,
          equal = Solved equal
        }
  Float {irrefutable, names, startPosition, float, evidence, equal} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    equal <- Unify.solveEvidence startPosition equal
    pure
      Solved.Float
        { irrefutable,
          names,
          float,
          startPosition,
          evidence = Solved evidence,
          equal = Solved equal
        }
  Character {irrefutable, names, startPosition, character} ->
    pure $
      Solved.Character
        { irrefutable,
          names,
          startPosition,
          character
        }
  String {irrefutable, names, startPosition, string} ->
    pure $
      Solved.String
        { irrefutable,
          names,
          startPosition,
          string
        }
