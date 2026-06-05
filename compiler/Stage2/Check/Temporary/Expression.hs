module Stage2.Check.Temporary.Expression where

import Control.Monad.ST (ST)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import qualified Data.Strict.Vector2 as Strict.Vector2
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( unsupportedFeatureListComprehension,
    unsupportedFeatureRecordUpdate,
    unsupportedFeatureRunST,
  )
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.Expression (Explicit (..))
import qualified Stage2.Tree.Expression as Solved
import qualified Stage2.Tree.Expression as Stage2 (Expression (..))
import qualified Stage2.Check.ConstructorInstance as ConstructorInstance
import Stage2.Check.Context (Context (..))
import Stage2.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage2.Check.DataInstance as DataInstance
import qualified Stage2.Check.TypeAnnotation as Annotation
import qualified Stage2.Check.TypeBinding as TypeBinding
import qualified Stage2.Check.Simple.Data as Simple.Data
import Stage2.Check.Simple.Scheme (instanciate)
import Stage2.Check.Temporary.Alternative (Alternative)
import qualified Stage2.Check.Temporary.Alternative as Alternative
import Stage2.Check.Temporary.CallHead (CallHead)
import qualified Stage2.Check.Temporary.CallHead as CallHead
import Stage2.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Stage2.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Stage2.Check.Temporary.Declaration as Declaration
import Stage2.Check.Temporary.Declarations (Declarations)
import qualified Stage2.Check.Temporary.Declarations as Declarations
import Stage2.Check.Temporary.Do (Do)
import qualified Stage2.Check.Temporary.Do as Do
import Stage2.Check.Temporary.ExpressionField (Field)
import qualified Stage2.Check.Temporary.ExpressionField as Field
import Stage2.Check.Temporary.Lambda (Lambda)
import qualified Stage2.Check.Temporary.Lambda as Lambda
import Stage2.Check.Temporary.Pattern (Pattern)
import qualified Stage2.Check.Temporary.Pattern as Pattern
import Stage2.Check.Temporary.RightHandSide (RightHandSide)
import qualified Stage2.Check.Temporary.RightHandSide as RightHandSide
import Stage2.Check.Go.Scheme (Scheme)
import qualified Stage2.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} Stage4.Tree.TypeDeclaration (assumeData)
import Prelude hiding (Bool (False, True))

data Expression s scope
  = CallHead
      { callHead :: !(CallHead s scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope)
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope)
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !Text
      }
  | Tuple
      { startPosition :: !Position,
        elements :: !(Strict.Vector2 (Expression s scope))
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector (Expression s scope))
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        constructorInfo :: !(ConstructorInfo s scope),
        fields :: !(Strict.Vector (Field s scope))
      }
  | Call
      { function :: !(Expression s scope),
        argument :: !(Expression s scope)
      }
  | Let
      { declarations :: !(Declarations Local s (Scope.Declaration ':+ scope)),
        letBody :: !(Expression s (Scope.Declaration ':+ scope))
      }
  | If
      { conditionx :: !(Expression s scope),
        thenx :: !(Expression s scope),
        elsex :: !(Expression s scope)
      }
  | Case
      { startPosition :: !Position,
        scrutinee :: !(Expression s scope),
        cases :: !(Strict.Vector (Alternative s scope))
      }
  | Lambda
      { startPosition :: !Position,
        parameter :: !(Pattern s scope),
        body :: !(Lambda s (Scope.Pattern ':+ scope))
      }
  | LambdaCase
      { startPosition :: !Position,
        cases :: !(Strict.Vector (Alternative s scope))
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide s scope))
      }
  | Do
      { startPosition :: !Position,
        statements :: !(Do s scope)
      }
  | Annotation
      { expression :: !(Unify.SchemeOver Expression s scope),
        operatorPosition :: !Position,
        annotation :: !(Scheme Position Check scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | RightSection
      { operatorPosition :: !Position,
        left :: !(CallHead s scope),
        right :: !(Expression s scope)
      }

check :: Context s scope -> Unify.Type s scope -> Stage2.Expression Group Resolve scope -> ST s (Expression s scope)
check context typex Stage2.CallHead {callHead} = do
  callHead <- CallHead.check context typex callHead
  pure CallHead {callHead}
check
  context@Context {typeEnvironment}
  typex
  Stage2.Record
    { constructorPosition,
      constructor,
      fields
    } =
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
          lookup index = entries Strict.Vector.! index
      Unify.unify context constructorPosition typex base
      fields <- traverse (Field.check context lookup) fields
      pure $ Record {constructorPosition, constructor, fields, constructorInfo}
check context typex Stage2.List {startPosition, items} = do
  inner <- Unify.fresh Unify.typex
  Unify.unify context startPosition typex (Unify.listWith inner)
  items <- traverse (check context inner) items
  pure List {startPosition, items}
check context resultType Stage2.Call {function, argument} = do
  argumentType <- Unify.fresh Unify.typex
  function1 <- check context (Unify.function argumentType resultType) function
  argument <- check context argumentType argument
  pure (Call function1 argument)
check context typex Stage2.Let {declarations, letBody} = do
  (context, declarations) <- Declarations.check context declarations
  letBody <- check context (shift typex) letBody
  pure (Let declarations letBody)
check context typex Stage2.If {condition, thenx, elsex} = do
  condition <- check context Unify.bool condition
  thenx <- check context typex thenx
  elsex <- check context typex elsex
  pure (If condition thenx elsex)
check context typex Stage2.MultiwayIf {branches} = do
  branches <- traverse (RightHandSide.check context typex) branches
  pure (MultiwayIf branches)
check context typex Stage2.Integer {startPosition, integer} = do
  evidence <- Unify.constrain context startPosition Type2.Num typex
  pure $ Integer {startPosition, integer, evidence}
check context typex Stage2.Float {startPosition, float} = do
  evidence <- Unify.constrain context startPosition Type2.Fractional typex
  pure $ Float {startPosition, float, evidence}
check context typex Stage2.String {startPosition, string} = do
  Unify.unify context startPosition typex (Unify.listWith Unify.char)
  pure $ String {startPosition, string}
check context typex Stage2.Character {startPosition, character} = do
  Unify.unify context startPosition typex Unify.char
  pure $ Character {startPosition, character}
check context typex Stage2.Tuple {startPosition, elements} = do
  items <- for elements $ \element -> do
    typex <- Unify.fresh Unify.typex
    element <- check context typex element
    pure (typex, element)
  let (types, elements) = Strict.Vector2.unzip items
  let target = foldl Unify.call (Unify.tuple $ length elements) types
  Unify.unify context startPosition typex target
  pure $ Tuple {startPosition, elements}
check _ _ Stage2.Comprehension {startPosition} =
  unsupportedFeatureListComprehension startPosition
check _ _ Stage2.Update {updatePosition} =
  unsupportedFeatureRecordUpdate updatePosition
check context typex Stage2.Case {startPosition, scrutinee, cases} = do
  binder <- Unify.fresh Unify.typex
  scrutinee <- check context binder scrutinee
  cases <- traverse (Alternative.check context typex binder) cases
  pure Case {startPosition, scrutinee, cases}
check context typex Stage2.Do {startPosition, dox} = do
  statements <- Do.check context typex dox
  pure Do {startPosition, statements}
check context typex Stage2.Lambda {startPosition, parameter, body} = do
  parameterType <- Unify.fresh Unify.typex
  parameter <- Pattern.check context parameterType parameter
  resultType <- Unify.fresh Unify.typex
  body <- Lambda.check (Pattern.augment parameter context) (shift resultType) body
  Unify.unify context startPosition typex (Unify.function parameterType resultType)
  pure Lambda {startPosition, parameter, body}
check context typex Stage2.LambdaCase {startPosition, cases} = do
  parameterType <- Unify.fresh Unify.typex
  resultType <- Unify.fresh Unify.typex
  cases <- traverse (Alternative.check context resultType parameterType) cases
  Unify.unify context startPosition typex (Unify.function parameterType resultType)
  pure LambdaCase {startPosition, cases}
check context typex Stage2.RightSection {left, operatorPosition, right} = do
  argumentType1 <- Unify.fresh Unify.typex
  argumentType2 <- Unify.fresh Unify.typex
  result <- Unify.fresh Unify.typex
  let target = argumentType1 `Unify.function` argumentType2 `Unify.function` result
  left <- CallHead.check context target left
  right <- check context argumentType2 right
  Unify.unify context operatorPosition typex (argumentType1 `Unify.function` result)
  pure RightSection {operatorPosition, left, right}
check context typex Stage2.Annotation {expression = Explicit expression, operatorPosition, annotation} = do
  Annotation.Annotation {annotation, annotation'} <- Annotation.checkAnnotation context annotation
  expression <- Declaration.checkAnnotation context operatorPosition annotation $
    \context typex -> check context typex expression
  (typex', instanciation) <- instanciate context operatorPosition annotation'
  Unify.unify context operatorPosition typex typex'
  pure Annotation {expression, operatorPosition, annotation, instanciation}
check _ _ Stage2.RunST {startPosition} =
  unsupportedFeatureRunST startPosition

solve :: Expression s scope -> Unify.Solve s (Solved.Expression Group Check scope)
solve = \case
  CallHead {callHead} -> do
    callHead <- CallHead.solve callHead
    pure Solved.CallHead {callHead}
  Record {constructorPosition, constructor, constructorInfo, fields} -> do
    fields <- traverse Field.solve fields
    constructorInfo <- ConstructorInfo.solve constructorInfo
    pure
      Solved.Record
        { constructorPosition,
          constructor,
          constructorInfo = Solved constructorInfo,
          fields
        }
  List {startPosition, items} -> do
    items <- traverse solve items
    pure $ Solved.List {startPosition, items}
  Call function argument -> do
    function <- solve function
    argument <- solve argument
    pure $ Solved.Call {function, argument}
  Let declarations body -> do
    declarations <- Declarations.solve declarations
    body <- solve body
    pure $ Solved.Let {declarations, letBody = body}
  If condition true false -> do
    condition <- solve condition
    true <- solve true
    false <- solve false
    pure $ Solved.If {condition, thenx = true, elsex = false}
  Case {startPosition, scrutinee, cases} -> do
    scrutinee <- solve scrutinee
    cases <- traverse Alternative.solve cases
    pure Solved.Case {startPosition, scrutinee, cases}
  Lambda {startPosition, parameter, body} -> do
    parameter <- Pattern.solve parameter
    body <- Lambda.solve body
    pure $ Solved.Lambda {startPosition, parameter, body}
  LambdaCase {startPosition, cases} -> do
    cases <- traverse Alternative.solve cases
    pure Solved.LambdaCase {startPosition, cases}
  MultiwayIf branches -> do
    branches <- traverse RightHandSide.solve branches
    pure $ Solved.MultiwayIf {branches}
  Integer {startPosition, integer, evidence} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    pure Solved.Integer {startPosition, integer, evidence = Solved evidence}
  Float {startPosition, float, evidence} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    pure Solved.Float {startPosition, float, evidence = Solved evidence}
  String {startPosition, string} -> pure $ Solved.String {startPosition, string}
  Character {startPosition, character} -> pure $ Solved.Character {startPosition, character}
  Tuple {startPosition, elements} -> do
    elements <- traverse solve elements
    pure $ Solved.Tuple {startPosition, elements}
  Do {startPosition, statements} -> do
    dox <- Do.solve statements
    pure Solved.Do {startPosition, dox}
  Annotation {expression, operatorPosition, annotation, instanciation} -> do
    expression <- Unify.solveSchemeOver (Unify.SolveScheme $ const solve) operatorPosition expression
    instanciation <- Unify.solveInstanciation operatorPosition instanciation
    pure
      Solved.Annotation
        { operatorPosition,
          expression = Known expression,
          annotation,
          instanciation = Solved instanciation
        }
  RightSection {operatorPosition, left, right} -> do
    left <- CallHead.solve left
    right <- solve right
    pure Solved.RightSection {operatorPosition, left, right}
