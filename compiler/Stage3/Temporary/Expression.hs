module Stage3.Temporary.Expression where

import Control.Monad.ST (ST)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import qualified Data.Strict.Vector2 as Strict.Vector2
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( unsupportedFeatureCaseExpressions,
    unsupportedFeatureDoNotation,
    unsupportedFeatureExpressionAnnotation,
    unsupportedFeatureFloatingPointLiterals,
    unsupportedFeatureLambdaCase,
    unsupportedFeatureLambdas,
    unsupportedFeatureListComprehension,
    unsupportedFeatureRecordUpdate,
    unsupportedFeatureRightSection,
    unsupportedFeatureRunST,
  )
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Table.Term as Term ((!))
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Term as Term (Index)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (shift)
import qualified Stage2.Tree.Expression as Stage2 (Expression (..))
import qualified Stage2.Tree.Selector as Redirect
import Stage3.Check.ClassInstance (ClassInstance (ClassInstance))
import qualified Stage3.Check.ClassInstance as ClassInstance
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import Stage3.Check.TermBinding (TermBinding (..), Type (..))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Builtin as Builtin
import qualified Stage3.Simple.Class as Simple.Class
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Simple.Scheme (instanciate)
import {-# SOURCE #-} Stage3.Simple.TypeDeclaration (assumeClass, assumeData)
import Stage3.Temporary.Declarations (Declarations)
import qualified Stage3.Temporary.Declarations as Declarations
import Stage3.Temporary.ExpressionField (Field)
import qualified Stage3.Temporary.ExpressionField as Field
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Tree.Expression as Solved
import qualified Stage3.Unify as Unify
import Prelude hiding (Bool (False, True))

data Expression s scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        parameters :: !Int
      }
  | Selector
      { selector :: !(Selector.Index scope),
        uniform :: !(Redirect.Uniform)
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Unify.Evidence s scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | Integer
      { integer :: !Integer,
        evidence :: !(Unify.Evidence s scope)
      }
  | Character
      { character :: !Char
      }
  | String {string :: !Text}
  | Tuple
      { elements :: !(Strict.Vector2 (Expression s scope))
      }
  | List {items :: !(Strict.Vector (Expression s scope))}
  | Record
      { constructor :: !(Constructor.Index scope),
        parameters :: !Int,
        fields :: !(Strict.Vector (Field s scope))
      }
  | Call
      { function :: !(Expression s scope),
        argument :: !(Expression s scope)
      }
  | Let
      { declarations :: !(Declarations s (Scope.Declaration ':+ scope)),
        letBody :: !(Expression s (Scope.Declaration ':+ scope))
      }
  | If
      { conditionx :: !(Expression s scope),
        thenx :: !(Expression s scope),
        elsex :: !(Expression s scope)
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide s scope))
      }

check :: Context s scope -> Unify.Type s scope -> Stage2.Expression scope -> ST s (Expression s scope)
check context@Context {termEnvironment} typex Stage2.Variable {Stage2.variablePosition, Stage2.variable} = do
  let TermBinding binding = termEnvironment Term.! variable
  binding >>= \case
    Wobbly scheme -> do
      (typex', instanciation) <- Unify.instanciate context variablePosition (Unify.monoScheme scheme)
      Unify.unify context variablePosition typex typex'
      pure Variable {variable, instanciation}
    Rigid scheme -> do
      (typex', instanciation) <- instanciate context variablePosition scheme
      Unify.unify context variablePosition typex typex'
      pure Variable {variable, instanciation}
check context@Context {typeEnvironment} typex Stage2.Constructor {Stage2.constructorPosition, Stage2.constructor} =
  do
    let Constructor.Index typeIndex constructorIndex = constructor
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    DataInstance {DataInstance.types, DataInstance.constructors} <-
      Simple.Data.instanciate datax
    let root = Unify.constructor typeIndex
        base = foldl Unify.call root types
        ConstructorInstance {ConstructorInstance.entries} =
          constructors Strict.Vector.! constructorIndex
        typex' = foldr Unify.function base entries
    Unify.unify context constructorPosition typex typex'
    pure Constructor {constructor, parameters = length entries}
check
  context@Context {typeEnvironment}
  typex
  Stage2.Record
    { Stage2.constructorPosition,
      Stage2.constructor,
      Stage2.fields
    } =
    do
      let Constructor.Index typeIndex constructorIndex = constructor
      datax <- do
        let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
        Builtin.index pure get typeIndex
      DataInstance {DataInstance.types, DataInstance.constructors} <-
        Simple.Data.instanciate datax
      let root = Unify.constructor typeIndex
          base = foldl Unify.call root types
          ConstructorInstance {ConstructorInstance.entries} =
            constructors Strict.Vector.! constructorIndex
          lookup index = entries Strict.Vector.! index
      Unify.unify context constructorPosition typex base
      fields <- traverse (Field.check context lookup) fields
      pure $ Record {constructor, fields, parameters = length entries}
check
  context@Context {typeEnvironment}
  typex
  Stage2.Selector {Stage2.selectorPosition, Stage2.selector} = do
    let Selector.Index typeIndex selectorIndex = selector
    datax <- do
      let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
      Builtin.index pure get typeIndex
    DataInstance {DataInstance.types, DataInstance.constructors, DataInstance.selectors} <-
      Simple.Data.instanciate datax
    let root = Unify.constructor typeIndex
        base = foldl Unify.call root types
        Redirect.Selector {Redirect.first, Redirect.index, Redirect.uniform} =
          selectors Strict.Vector.! selectorIndex
        ConstructorInstance {ConstructorInstance.entries} =
          constructors Strict.Vector.! first
        entry = entries Strict.Vector.! index
        typex' = Unify.function base entry
    Unify.unify context selectorPosition typex typex'
    pure Selector {selector, uniform}
check
  context@Context {typeEnvironment}
  typex
  Stage2.Method {Stage2.methodPosition, Stage2.method} = do
    case method of
      Method.Index typeIndex methodIndex -> do
        classx <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        ClassInstance {ClassInstance.methods, ClassInstance.evidence} <-
          Simple.Class.instanciate context methodPosition typeIndex classx
        let method' = methods Strict.Vector.! methodIndex
        (typex', instanciation) <- Unify.instanciate context methodPosition method'
        Unify.unify context methodPosition typex typex'
        pure Method {method, evidence, instanciation}
check context typex Stage2.List {Stage2.startPosition, Stage2.items} = do
  inner <- Unify.fresh Unify.typex
  Unify.unify context startPosition typex (Unify.listWith inner)
  items <- traverse (check context inner) items
  pure (List items)
check context resultType Stage2.Call {Stage2.function, Stage2.argument} = do
  argumentType <- Unify.fresh Unify.typex
  function1 <- check context (Unify.function argumentType resultType) function
  argument <- check context argumentType argument
  pure (Call function1 argument)
check context typex Stage2.Let {Stage2.declarations, Stage2.letBody} = do
  (context, declarations) <- Declarations.check context declarations
  letBody <- check context (shift typex) letBody
  pure (Let declarations letBody)
check context typex Stage2.If {Stage2.condition, Stage2.thenx, Stage2.elsex} = do
  condition <- check context Unify.bool condition
  thenx <- check context typex thenx
  elsex <- check context typex elsex
  pure (If condition thenx elsex)
check context typex Stage2.MultiwayIf {Stage2.branches} = do
  branches <- traverse (RightHandSide.check context typex) branches
  pure (MultiwayIf branches)
check context typex Stage2.Integer {Stage2.startPosition, Stage2.integer} = do
  evidence <- Unify.constrain context startPosition Type2.Num typex
  pure $ Integer {integer, evidence}
check context typex Stage2.String {Stage2.startPosition, Stage2.string} = do
  Unify.unify context startPosition typex (Unify.listWith Unify.char)
  pure $ String string
check context typex Stage2.Character {Stage2.startPosition, Stage2.character} = do
  Unify.unify context startPosition typex Unify.char
  pure $ Character {character}
check context typex Stage2.Tuple {Stage2.startPosition, Stage2.elements} = do
  items <- for elements $ \element -> do
    typex <- Unify.fresh Unify.typex
    element <- check context typex element
    pure (typex, element)
  let (types, elements) = Strict.Vector2.unzip items
  let target = foldl Unify.call (Unify.tuple $ length elements) types
  Unify.unify context startPosition typex target
  pure $ Tuple {elements}
check _ _ Stage2.Float {Stage2.startPosition} =
  unsupportedFeatureFloatingPointLiterals startPosition
check _ _ Stage2.Comprehension {Stage2.startPosition} =
  unsupportedFeatureListComprehension startPosition
check _ _ Stage2.Update {Stage2.updatePosition} =
  unsupportedFeatureRecordUpdate updatePosition
check _ _ Stage2.Case {Stage2.startPosition} =
  unsupportedFeatureCaseExpressions startPosition
check _ _ Stage2.Do {Stage2.startPosition} =
  unsupportedFeatureDoNotation startPosition
check _ _ Stage2.Lambda {Stage2.startPosition} =
  unsupportedFeatureLambdas startPosition
check _ _ Stage2.LambdaCase {Stage2.startPosition} =
  unsupportedFeatureLambdaCase startPosition
check _ _ Stage2.RightSectionVariable {Stage2.operatorPosition} =
  unsupportedFeatureRightSection operatorPosition
check _ _ Stage2.RightSectionMethod {Stage2.operatorPosition} =
  unsupportedFeatureRightSection operatorPosition
check _ _ Stage2.RightSectionSelector {Stage2.operatorPosition} =
  unsupportedFeatureRightSection operatorPosition
check _ _ Stage2.RightSectionConstructor {Stage2.operatorPosition} =
  unsupportedFeatureRightSection operatorPosition
check _ _ Stage2.RightSectionCons {Stage2.operatorPosition} =
  unsupportedFeatureRightSection operatorPosition
check _ _ Stage2.Annotation {Stage2.operatorPosition} =
  unsupportedFeatureExpressionAnnotation operatorPosition
check _ _ Stage2.RunST {Stage2.startPosition} =
  unsupportedFeatureRunST startPosition

solve :: Expression s scope -> ST s (Solved.Expression scope)
solve = \case
  Variable {variable, instanciation} -> do
    instanciation <- Unify.solveInstanciation instanciation
    pure
      Solved.Variable
        { Solved.variable,
          Solved.instanciation
        }
  Constructor {constructor, parameters} -> pure $ Solved.Constructor {Solved.constructor, Solved.parameters}
  Selector {selector, uniform} -> pure $ Solved.Selector {Solved.selector, Solved.uniform}
  Method {method, evidence, instanciation} -> do
    evidence <- Unify.solveEvidence evidence
    instanciation <- Unify.solveInstanciation instanciation
    pure $
      Solved.Method
        { Solved.method,
          Solved.evidence,
          Solved.instanciation
        }
  Record {constructor, parameters, fields} -> do
    fields <- traverse Field.solve fields
    pure Solved.Record {Solved.constructor, Solved.parameters, Solved.fields}
  List items -> do
    items <- traverse solve items
    pure $ Solved.List items
  Call function argument -> do
    function <- solve function
    argument <- solve argument
    pure $ Solved.Call function argument
  Let declarations body -> do
    declarations <- Declarations.solve declarations
    body <- solve body
    pure $ Solved.Let declarations body
  If condition true false -> do
    condition <- solve condition
    true <- solve true
    false <- solve false
    pure $ Solved.If condition true false
  MultiwayIf branches -> do
    branches <- traverse RightHandSide.solve branches
    pure $ Solved.MultiwayIf branches
  Integer {integer, evidence} -> do
    evidence <- Unify.solveEvidence evidence
    pure Solved.Integer {Solved.integer, Solved.evidence}
  String {string} -> pure $ Solved.String {Solved.string}
  Character {character} -> pure $ Solved.Character {Solved.character}
  Tuple {elements} -> do
    elements <- traverse solve elements
    pure $ Solved.Tuple {Solved.elements}
