module Stage3.Temporary.Pattern where

import Control.Monad (when)
import Control.Monad.ST (ST)
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
import qualified Stage2.Tree.Pattern as Stage2
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import Stage3.Check.TermBinding (TermBinding (TermBinding), Type (..))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage3.Temporary.ConstructorInfo as ConstructorInfo
import Stage3.Temporary.PatternField (Field)
import qualified Stage3.Temporary.PatternField as Field
import qualified Stage3.Tree.Pattern as Solved
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Builtin as Builtin
import Stage4.Tree.TypeDeclaration (assumeData)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern s scope
  = Wildcard
      { typex :: !(Unify.Type s scope)
      }
  | Constructor
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern s scope)),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Record
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field s scope)),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Integer
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Float
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Character
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        character :: !Char
      }
  | String
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        string :: !Text
      }
  | List
      { typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        items :: !(Strict.Vector1 (Pattern s scope))
      }

instance Unify.Zonk Pattern where
  zonk zonker = \case
    Wildcard {typex} -> do
      typex <- Unify.zonk zonker typex
      pure Wildcard {typex}
    Constructor {typex, irrefutable, constructor, patterns, constructorInfo} -> do
      typex <- Unify.zonk zonker typex
      patterns <- traverse (Unify.zonk zonker) patterns
      constructorInfo <- Unify.zonk zonker constructorInfo
      pure Constructor {typex, irrefutable, constructor, patterns, constructorInfo}
    Record {typex, irrefutable, constructor, fields, constructorInfo} -> do
      typex <- Unify.zonk zonker typex
      fields <- traverse (Unify.zonk zonker) fields
      constructorInfo <- Unify.zonk zonker constructorInfo
      pure Record {typex, irrefutable, constructor, fields, constructorInfo}
    Integer {typex, irrefutable, startPosition, integer, evidence, equal} -> do
      typex <- Unify.zonk zonker typex
      evidence <- Unify.zonk zonker evidence
      equal <- Unify.zonk zonker equal
      pure Integer {typex, startPosition, irrefutable, integer, evidence, equal}
    Character {typex, irrefutable, character} -> do
      typex <- Unify.zonk zonker typex
      pure Character {typex, character, irrefutable}
    Float {typex, irrefutable, startPosition, float, evidence, equal} -> do
      typex <- Unify.zonk zonker typex
      evidence <- Unify.zonk zonker evidence
      equal <- Unify.zonk zonker equal
      pure Float {typex, irrefutable, startPosition, float, evidence, equal}
    String {typex, irrefutable, string} -> do
      typex <- Unify.zonk zonker typex
      pure String {typex, irrefutable, string}
    List {typex, irrefutable, items} -> do
      typex <- Unify.zonk zonker typex
      items <- traverse (Unify.zonk zonker) items
      pure List {typex, irrefutable, items}

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
    at = TermBinding $ pure $ Wobbly (shift (typex patternx))
    select = case patternx of
      Wildcard {} -> Strict.Vector.empty
      List {items} -> Strict.Vector.map augmentPattern $ Strict.Vector1.toVector items
      Constructor {patterns} -> fmap augmentPattern patterns
      Record {fields} -> fmap Field.augmentField fields
      Integer {} -> Strict.Vector.empty
      Float {} -> Strict.Vector.empty
      Character {} -> Strict.Vector.empty
      String {} -> Strict.Vector.empty

check :: Context s scope -> Unify.Type s scope -> Stage2.Pattern scope -> ST s (Pattern s scope)
check context@Context {typeEnvironment} typex = \case
  Stage2.Wildcard {} -> pure Wildcard {typex}
  Stage2.Constructor
    { irrefutable,
      constructorPosition,
      constructor,
      patterns
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
        pure $ Constructor {typex, irrefutable, constructor, patterns, constructorInfo}
  Stage2.Record {irrefutable, constructorPosition, constructor, fields} -> do
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
    pure $ Record {typex, irrefutable, constructor, fields, constructorInfo}
  Stage2.List {irrefutable, startPosition, items} -> do
    element <- Unify.fresh Unify.typex
    items <- traverse (check context element) items
    Unify.unify context startPosition typex (Unify.listWith element)
    pure $ List {typex, irrefutable, items}
  Stage2.Character {irrefutable, startPosition, character} -> do
    Unify.unify context startPosition typex Unify.char
    pure $ Character {typex, irrefutable, character}
  Stage2.String {irrefutable, startPosition, string} -> do
    Unify.unify context startPosition typex (Unify.listWith Unify.char)
    pure $ String {typex, irrefutable, string}
  Stage2.Integer {irrefutable, startPosition, integer} -> do
    evidence <- Unify.constrain context startPosition Type2.Num typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Integer {typex, irrefutable, startPosition, integer, evidence, equal}
  Stage2.Float {irrefutable, startPosition, float} -> do
    evidence <- Unify.constrain context startPosition Type2.Fractional typex
    equal <- Unify.constrain context startPosition Type2.Eq typex
    pure Float {typex, irrefutable, startPosition, float, evidence, equal}

solve :: Pattern s scope -> ST s (Solved.Pattern scope)
solve = \case
  Wildcard {} -> pure Solved.Wildcard {}
  Constructor {irrefutable, constructor, patterns, constructorInfo} -> do
    patterns <- traverse solve patterns
    constructorInfo <- ConstructorInfo.solve constructorInfo
    pure Solved.Constructor {irrefutable, constructor, patterns, constructorInfo}
  Record {irrefutable, constructor, fields, constructorInfo} -> do
    fields <- traverse Field.solve fields
    constructorInfo <- ConstructorInfo.solve constructorInfo
    pure Solved.Record {irrefutable, constructor, fields, constructorInfo}
  List {irrefutable, items} -> do
    items <- traverse solve items
    pure Solved.List {irrefutable, items}
  Integer {irrefutable, startPosition, integer, evidence, equal} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    equal <- Unify.solveEvidence startPosition equal
    pure Solved.Integer {irrefutable, integer, evidence, equal}
  Float {irrefutable, startPosition, float, evidence, equal} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    equal <- Unify.solveEvidence startPosition equal
    pure Solved.Float {irrefutable, float, evidence, equal}
  Character {irrefutable, character} -> pure $ Solved.Character {irrefutable, character}
  String {irrefutable, string} -> pure $ Solved.String {irrefutable, string}
