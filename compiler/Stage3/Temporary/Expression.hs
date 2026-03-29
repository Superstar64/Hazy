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
  ( unsupportedFeatureFloatingPointLiterals,
    unsupportedFeatureListComprehension,
    unsupportedFeatureRecordUpdate,
    unsupportedFeatureRunST,
  )
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (shift)
import qualified Stage2.Tree.Expression as Stage2 (Expression (..))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import qualified Stage3.Check.TypeAnnotation as Annotation
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Simple.Scheme (instanciate)
import Stage3.Temporary.Alternative (Alternative)
import qualified Stage3.Temporary.Alternative as Alternative
import Stage3.Temporary.CallHead (CallHead)
import qualified Stage3.Temporary.CallHead as CallHead
import Stage3.Temporary.Declarations (Declarations)
import qualified Stage3.Temporary.Declarations as Declarations
import Stage3.Temporary.Do (Do)
import qualified Stage3.Temporary.Do as Do
import Stage3.Temporary.ExpressionField (Field)
import qualified Stage3.Temporary.ExpressionField as Field
import Stage3.Temporary.Lambda (Lambda)
import qualified Stage3.Temporary.Lambda as Lambda
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import Stage3.Temporary.RightHandSide (RightHandSide)
import qualified Stage3.Temporary.RightHandSide as RightHandSide
import qualified Stage3.Temporary.TermDeclaration as TermDeclaration
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import qualified Stage3.Tree.Expression as Solved
import Stage3.Tree.Scheme (Scheme)
import qualified Stage3.Unify as Unify
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
        constructorInfo :: !ConstructorInfo,
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
  | Case
      { scrutinee :: !(Expression s scope),
        cases :: !(Strict.Vector (Alternative s scope))
      }
  | Lambda
      { parameter :: !(Pattern s scope),
        body :: !(Lambda s (Scope.Pattern ':+ scope))
      }
  | LambdaCase
      { cases :: !(Strict.Vector (Alternative s scope))
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide s scope))
      }
  | Do
      { statements :: !(Do s scope)
      }
  | Annotation
      { expression :: !(Unify.SchemeOver Expression s scope),
        operatorPosition :: !Position,
        annotation :: !(Scheme scope),
        instanciation :: !(Unify.Instanciation s scope)
      }
  | RightSection
      { left :: !(CallHead s scope),
        right :: !(Expression s scope)
      }

instance Unify.Zonk Expression where
  zonk zonker = \case
    CallHead {callHead} -> do
      callHead <- Unify.zonk zonker callHead
      pure CallHead {callHead}
    Integer {startPosition, integer, evidence} -> do
      evidence <- Unify.zonk zonker evidence
      pure Integer {startPosition, integer, evidence}
    Character {character} -> pure Character {character}
    String {string} -> pure String {string}
    Tuple {elements} -> do
      elements <- traverse (Unify.zonk zonker) elements
      pure Tuple {elements}
    List {items} -> do
      items <- traverse (Unify.zonk zonker) items
      pure List {items}
    Record {constructor, constructorInfo, fields} -> do
      fields <- traverse (Unify.zonk zonker) fields
      pure Record {constructor, constructorInfo, fields}
    Call {function, argument} -> do
      function <- Unify.zonk zonker function
      argument <- Unify.zonk zonker argument
      pure Call {function, argument}
    Let {declarations, letBody} -> do
      declarations <- Unify.zonk zonker declarations
      letBody <- Unify.zonk zonker letBody
      pure Let {declarations, letBody}
    If {conditionx, thenx, elsex} -> do
      conditionx <- Unify.zonk zonker conditionx
      thenx <- Unify.zonk zonker thenx
      elsex <- Unify.zonk zonker elsex
      pure If {conditionx, thenx, elsex}
    Case {scrutinee, cases} -> do
      scrutinee <- Unify.zonk zonker scrutinee
      cases <- traverse (Unify.zonk zonker) cases
      pure Case {scrutinee, cases}
    Lambda {parameter, body} -> do
      parameter <- Unify.zonk zonker parameter
      body <- Unify.zonk zonker body
      pure Lambda {parameter, body}
    LambdaCase {cases} -> do
      cases <- traverse (Unify.zonk zonker) cases
      pure LambdaCase {cases}
    MultiwayIf {branches} -> do
      branches <- traverse (Unify.zonk zonker) branches
      pure MultiwayIf {branches}
    Do {statements} -> do
      statements <- Unify.zonk zonker statements
      pure Do {statements}
    Annotation {expression, operatorPosition, annotation, instanciation} -> do
      expression <- Unify.zonk zonker expression
      instanciation <- Unify.zonk zonker instanciation
      pure Annotation {expression, operatorPosition, annotation, instanciation}
    RightSection {left, right} -> do
      left <- Unify.zonk zonker left
      right <- Unify.zonk zonker right
      pure RightSection {left, right}

check :: Context s scope -> Unify.Type s scope -> Stage2.Expression scope -> ST s (Expression s scope)
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
        Simple.Data.instanciate datax
      let root = Unify.constructor typeIndex
          base = foldl Unify.call root types
          instancex = constructors Strict.Vector.! constructorIndex
          entries = ConstructorInstance.types instancex
          constructorInfo = ConstructorInstance.info instancex
          lookup index = entries Strict.Vector.! index
      Unify.unify context constructorPosition typex base
      fields <- traverse (Field.check context lookup) fields
      pure $ Record {constructor, fields, constructorInfo}
check context typex Stage2.List {startPosition, items} = do
  inner <- Unify.fresh Unify.typex
  Unify.unify context startPosition typex (Unify.listWith inner)
  items <- traverse (check context inner) items
  pure (List items)
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
check context typex Stage2.String {startPosition, string} = do
  Unify.unify context startPosition typex (Unify.listWith Unify.char)
  pure $ String string
check context typex Stage2.Character {startPosition, character} = do
  Unify.unify context startPosition typex Unify.char
  pure $ Character {character}
check context typex Stage2.Tuple {startPosition, elements} = do
  items <- for elements $ \element -> do
    typex <- Unify.fresh Unify.typex
    element <- check context typex element
    pure (typex, element)
  let (types, elements) = Strict.Vector2.unzip items
  let target = foldl Unify.call (Unify.tuple $ length elements) types
  Unify.unify context startPosition typex target
  pure $ Tuple {elements}
check _ _ Stage2.Float {startPosition} =
  unsupportedFeatureFloatingPointLiterals startPosition
check _ _ Stage2.Comprehension {startPosition} =
  unsupportedFeatureListComprehension startPosition
check _ _ Stage2.Update {updatePosition} =
  unsupportedFeatureRecordUpdate updatePosition
check context typex Stage2.Case {scrutinee, cases} = do
  binder <- Unify.fresh Unify.typex
  scrutinee <- check context binder scrutinee
  cases <- traverse (Alternative.check context typex binder) cases
  pure Case {scrutinee, cases}
check context typex Stage2.Do {statements} = do
  statements <- Do.check context typex statements
  pure Do {statements}
check context typex Stage2.Lambda {startPosition, parameter, body} = do
  parameterType <- Unify.fresh Unify.typex
  parameter <- Pattern.check context parameterType parameter
  resultType <- Unify.fresh Unify.typex
  body <- Lambda.check (Pattern.augment parameter context) (shift resultType) body
  Unify.unify context startPosition typex (Unify.function parameterType resultType)
  pure Lambda {parameter, body}
check context typex Stage2.LambdaCase {startPosition, cases} = do
  parameterType <- Unify.fresh Unify.typex
  resultType <- Unify.fresh Unify.typex
  cases <- traverse (Alternative.check context resultType parameterType) cases
  Unify.unify context startPosition typex (Unify.function parameterType resultType)
  pure LambdaCase {cases}
check context typex Stage2.RightSection {left, operatorPosition, right} = do
  argumentType1 <- Unify.fresh Unify.typex
  argumentType2 <- Unify.fresh Unify.typex
  result <- Unify.fresh Unify.typex
  let target = argumentType1 `Unify.function` argumentType2 `Unify.function` result
  left <- CallHead.check context target left
  right <- check context argumentType2 right
  Unify.unify context operatorPosition typex (argumentType1 `Unify.function` result)
  pure RightSection {left, right}
check context typex Stage2.Annotation {expression, operatorPosition, annotation} = do
  Annotation.Annotation {annotation, annotation'} <- Annotation.checkAnnotation context annotation
  expression <- TermDeclaration.checkAnnotation context operatorPosition annotation $
    \context typex -> check context typex expression
  (typex', instanciation) <- instanciate context operatorPosition annotation'
  Unify.unify context operatorPosition typex typex'
  pure Annotation {expression, operatorPosition, annotation, instanciation}
check _ _ Stage2.RunST {startPosition} =
  unsupportedFeatureRunST startPosition

solve :: Expression s scope -> ST s (Solved.Expression scope)
solve = \case
  CallHead {callHead} -> do
    callHead <- CallHead.solve callHead
    pure Solved.CallHead {callHead}
  Record {constructor, constructorInfo, fields} -> do
    fields <- traverse Field.solve fields
    pure Solved.Record {constructor, constructorInfo, fields}
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
  Case {scrutinee, cases} -> do
    scrutinee <- solve scrutinee
    cases <- traverse Alternative.solve cases
    pure Solved.Case {scrutinee, cases}
  Lambda {parameter, body} -> do
    parameter <- Pattern.solve parameter
    body <- Lambda.solve body
    pure $ Solved.Lambda {parameter, body}
  LambdaCase {cases} -> do
    cases <- traverse Alternative.solve cases
    pure Solved.LambdaCase {cases}
  MultiwayIf branches -> do
    branches <- traverse RightHandSide.solve branches
    pure $ Solved.MultiwayIf branches
  Integer {startPosition, integer, evidence} -> do
    evidence <- Unify.solveEvidence startPosition evidence
    pure Solved.Integer {integer, evidence}
  String {string} -> pure $ Solved.String {string}
  Character {character} -> pure $ Solved.Character {character}
  Tuple {elements} -> do
    elements <- traverse solve elements
    pure $ Solved.Tuple {elements}
  Do {statements} -> do
    statements <- Do.solve statements
    pure Solved.Do {statements}
  Annotation {expression, operatorPosition, annotation, instanciation} -> do
    expression <- Unify.solveSchemeOver (Unify.Solve $ const solve) operatorPosition expression
    instanciation <- Unify.solveInstanciation operatorPosition instanciation
    pure Solved.Annotation {expression, annotation, instanciation}
  RightSection {left, right} -> do
    left <- CallHead.solve left
    right <- solve right
    pure Solved.RightSection {left, right}
