module Stage2.Check.Temporary.Pattern where

import Control.Monad (when)
import Control.Monad.ST (ST)
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
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type
import Stage2.Index.Term (Bound)
import qualified Stage2.Index.Term as Bound (Bound (..))
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..))
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Pattern as Solved
import qualified Stage2.Tree.Pattern as Stage2
import qualified Stage2.Check.ConstructorInstance as ConstructorInstance
import Stage2.Check.Context (Context (..))
import Stage2.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage2.Check.DataInstance as DataInstance
import Stage2.Check.TermBinding (TermBinding (TermBinding), Type (..))
import qualified Stage2.Check.TypeBinding as TypeBinding
import qualified Stage2.Check.Simple.Data as Simple.Data
import Stage2.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage2.Check.Temporary.ConstructorInfo as ConstructorInfo
import Stage2.Check.Temporary.PatternField (Field)
import qualified Stage2.Check.Temporary.PatternField as Field
import qualified Stage2.Unify as Unify
import qualified Stage4.Tree.Builtin as Builtin
import Stage4.Tree.TypeDeclaration (assumeData)
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

check :: Context s scope -> Unify.Type s scope -> Stage2.Pattern Resolve scope -> ST s (Pattern s scope)
check context@Context {typeEnvironment} typex = \case
  Stage2.Wildcard {names} -> pure Wildcard {typex, names}
  Stage2.Constructor
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
  Stage2.Record
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
  Stage2.List {irrefutable, names, startPosition, items} -> do
    element <- Unify.fresh Unify.typex
    items <- traverse (check context element) items
    Unify.unify context startPosition typex (Unify.listWith element)
    pure $ List {typex, names, startPosition, irrefutable, items}
  Stage2.Character {irrefutable, names, startPosition, character} -> do
    Unify.unify context startPosition typex Unify.char
    pure $ Character {typex, names, startPosition, irrefutable, character}
  Stage2.String {irrefutable, names, startPosition, string} -> do
    Unify.unify context startPosition typex (Unify.listWith Unify.char)
    pure $ String {typex, names, startPosition, irrefutable, string}
  Stage2.Integer {irrefutable, names, startPosition, integer} -> do
    evidence <- Unify.constrain context startPosition Type2.Num typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Integer {typex, names, irrefutable, startPosition, integer, evidence, equal}
  Stage2.Float {irrefutable, names, startPosition, float} -> do
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
