{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Expression where

import Data.Foldable (toList)
import Data.List.Reverse (List (..))
import qualified Data.List.Reverse as Reverse
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import Error (badRunSTCall)
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import qualified Stage1.Tree.Expression as Stage1 (Expression (..))
import qualified Stage1.Tree.ExpressionField as Stage1 (Field (Field, Pun))
import qualified Stage1.Tree.ExpressionField as Stage1.Field
import qualified Stage1.Tree.ExpressionInfix as Stage1.Infix
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (QualifiedName (..))
import qualified Stage2.Index.Constructor as Constructor (Index (..), cons, tuple)
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term as Term (Index (..))
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Resolve.Binding.Term as Term (Binding (..))
import Stage2.Resolve.Context
  ( Context (..),
    (!-),
    (!-%),
    (!-*),
    (!=),
    (!=*~),
    (!=~),
  )
import Stage2.Scope as Null (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage2.Temporary.ExpressionInfix as Infix (fix, fixWith, resolve)
import Stage2.Tree.Alternative (Alternative (..))
import qualified Stage2.Tree.Alternative as Alternative (resolve)
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations
import Stage2.Tree.ExpressionField (Field)
import qualified Stage2.Tree.ExpressionField as Field (resolve)
import Stage2.Tree.Lambda (Lambda (..))
import qualified Stage2.Tree.Lambda as Lambda (Resolve (..), resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.RightHandSide as RightHandSide (resolve)
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Scheme (augment, resolve)
import Stage2.Tree.Select (Select (..))
import qualified Stage2.Tree.Select as Select (resolve)
import Stage2.Tree.Statements (Statements)
import qualified Stage2.Tree.Statements as Statements (resolve)
import Prelude hiding (Bool (False, True), Either (Left, Right))

data Expression scope
  = Variable
      { variablePosition :: !Position,
        variable :: !(Term.Index scope)
      }
  | Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope)
      }
  | Selector
      { selectorPosition :: !Position,
        selector :: !(Selector.Index scope)
      }
  | Method
      { methodPosition :: !Position,
        method :: !(Method.Index scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational
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
        elements :: !(Strict.Vector2 (Expression scope))
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector (Expression scope))
      }
  | Comprehension
      { startPosition :: !Position,
        statements :: !(Statements scope)
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope))
      }
  | Update
      { base :: !(Expression scope),
        updatePosition :: !Position,
        updates :: !(Strict.Vector1 (Select scope))
      }
  | Call
      { function :: !(Expression scope),
        argument :: !(Expression scope)
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Expression (Scope.Declaration ':+ scope))
      }
  | If
      { condition :: !(Expression scope),
        thenx :: !(Expression scope),
        elsex :: !(Expression scope)
      }
  | MultiwayIf
      { branches :: !(Strict.Vector1 (RightHandSide scope))
      }
  | Case
      { startPosition :: !Position,
        scrutinee :: !(Expression scope),
        cases :: !(Strict.Vector (Alternative scope))
      }
  | Do
      { startPosition :: !Position,
        statements :: !(Statements scope)
      }
  | Lambda
      { startPosition :: !Position,
        parameter :: !(Pattern scope),
        body :: !(Lambda (Scope.Pattern ':+ scope))
      }
  | LambdaCase
      { startPosition :: !Position,
        cases :: !(Strict.Vector (Alternative scope))
      }
  | RightSectionVariable
      { operatorPosition :: !Position,
        leftVariable :: !(Term.Index scope),
        right :: !(Expression scope)
      }
  | RightSectionConstructor
      { operatorPosition :: !Position,
        leftConstructor :: !(Constructor.Index scope),
        right :: !(Expression scope)
      }
  | RightSectionSelector
      { operatorPosition :: !Position,
        leftSelector :: !(Selector.Index scope),
        right :: !(Expression scope)
      }
  | RightSectionMethod
      { operatorPosition :: !Position,
        leftMethod :: !(Method.Index scope),
        right :: !(Expression scope)
      }
  | RightSectionCons
      { operatorPosition :: !Position,
        right :: !(Expression scope)
      }
  | Annotation
      { expression :: !(Expression (Scope.Local ':+ scope)),
        operatorPosition :: !Position,
        annotation :: !(Scheme Position scope)
      }
  | RunST
      { startPosition :: !Position,
        imperative :: !(Expression scope)
      }
  deriving (Show)

instance Shift Expression where
  shift = shiftDefault

instance Shift.Functor Expression where
  map category = \case
    Variable {variablePosition, variable} ->
      Variable
        { variablePosition,
          variable = Shift.map category variable
        }
    Constructor {constructorPosition, constructor} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor
        }
    Selector {selectorPosition, selector} ->
      Selector
        { selectorPosition,
          selector = Shift.map category selector
        }
    Method {methodPosition, method} ->
      Method
        { methodPosition,
          method = Shift.map category method
        }
    Integer {startPosition, integer} -> Integer {startPosition, integer}
    Float {startPosition, float} -> Float {startPosition, float}
    Character {startPosition, character} -> Character {startPosition, character}
    String {startPosition, string} -> String {startPosition, string}
    Tuple {startPosition, elements} ->
      Tuple
        { startPosition,
          elements = fmap (Shift.map category) elements
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = fmap (Shift.map category) items
        }
    Comprehension {startPosition, statements} ->
      Comprehension
        { startPosition,
          statements = Shift.map category statements
        }
    Record {constructorPosition, constructor, fields} ->
      Record
        { constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields
        }
    Update {base, updatePosition, updates} ->
      Update
        { base = Shift.map category base,
          updatePosition,
          updates = fmap (Shift.map category) updates
        }
    Call {function, argument} ->
      Call
        { function = Shift.map category function,
          argument = Shift.map category argument
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Shift.map (Shift.Over category) declarations,
          letBody = Shift.map (Shift.Over category) letBody
        }
    If {condition, thenx, elsex} ->
      If
        { condition = Shift.map category condition,
          thenx = Shift.map category thenx,
          elsex = Shift.map category elsex
        }
    MultiwayIf {branches} ->
      MultiwayIf
        { branches = fmap (Shift.map category) branches
        }
    Case {startPosition, scrutinee, cases} ->
      Case
        { startPosition,
          scrutinee = Shift.map category scrutinee,
          cases = fmap (Shift.map category) cases
        }
    Do {startPosition, statements} ->
      Do
        { startPosition,
          statements = Shift.map category statements
        }
    Lambda {startPosition, parameter, body} ->
      Lambda
        { startPosition,
          parameter = Shift.map category parameter,
          body = Shift.map (Shift.Over category) body
        }
    LambdaCase {startPosition, cases} ->
      LambdaCase
        { startPosition,
          cases = fmap (Shift.map category) cases
        }
    RightSectionVariable {operatorPosition, right, leftVariable} ->
      RightSectionVariable
        { operatorPosition,
          leftVariable = Shift.map category leftVariable,
          right = Shift.map category right
        }
    RightSectionConstructor {operatorPosition, right, leftConstructor} ->
      RightSectionConstructor
        { operatorPosition,
          right = Shift.map category right,
          leftConstructor = Shift.map category leftConstructor
        }
    RightSectionSelector {operatorPosition, right, leftSelector} ->
      RightSectionSelector
        { operatorPosition,
          leftSelector = Shift.map category leftSelector,
          right = Shift.map category right
        }
    RightSectionMethod {operatorPosition, right, leftMethod} ->
      RightSectionMethod
        { operatorPosition,
          leftMethod = Shift.map category leftMethod,
          right = Shift.map category right
        }
    RightSectionCons {operatorPosition, right} ->
      RightSectionCons
        { operatorPosition,
          right = Shift.map category right
        }
    Annotation {expression, operatorPosition, annotation} ->
      Annotation
        { expression = Shift.map (Shift.Over category) expression,
          operatorPosition,
          annotation = Shift.map category annotation
        }
    RunST {startPosition, imperative} ->
      RunST
        { startPosition,
          imperative = Shift.map category imperative
        }

variablex :: Position -> Term2.Index scope -> Expression scope
variablex variablePosition@selectorPosition@methodPosition = \case
  Term2.Index variable ->
    Variable
      { variablePosition,
        variable
      }
  Term2.Select selector ->
    Selector
      { selectorPosition,
        selector
      }
  Term2.Method method ->
    Method
      { methodPosition,
        method
      }
  Term2.RunST -> badRunSTCall variablePosition

resolveTerm2 :: Position -> Term2.Index scope -> Reverse.List (Expression scope) -> Expression scope
resolveTerm2 position index Nil = variablex position index
resolveTerm2 startPosition Term2.RunST (Nil :> imperative) =
  RunST
    { startPosition,
      imperative
    }
resolveTerm2 position index (arguments :> argument) =
  Call
    { function = resolveTerm2 position index arguments,
      argument
    }

resolveConstructor2 :: Position -> Constructor.Index scope -> Reverse.List (Expression scope) -> Expression scope
resolveConstructor2 constructorPosition constructor Nil =
  Constructor
    { constructorPosition,
      constructor
    }
resolveConstructor2 position index (arguments :> argument) =
  Call
    { function = resolveConstructor2 position index arguments,
      argument
    }

resolve :: Context scope -> Stage1.Expression Position -> Expression scope
resolve context expression = resolveWith context expression []

resolveWith :: Context scope -> Stage1.Expression Position -> [Expression scope] -> Expression scope
resolveWith context Stage1.Variable {variable = startPosition :@ variable} arguments =
  resolveTerm2 startPosition (context !-* startPosition :@ variable) (Reverse.fromList arguments)
resolveWith context Stage1.Constructor {constructor = startPosition :@ constructor} arguments =
  resolveConstructor2 startPosition (context !=*~ startPosition :@ constructor) (Reverse.fromList arguments)
resolveWith context Stage1.Call {function, argument} arguments
  | argument <- resolve context argument =
      resolveWith context function (argument : arguments)
resolveWith context expression arguments@(_ : _)
  | expression <- resolve context expression = foldl call expression arguments
  where
    call function argument =
      Call
        { function,
          argument
        }
resolveWith context expression [] = case expression of
  Stage1.Integer {startPosition, integer} -> Integer {startPosition, integer}
  Stage1.Float {startPosition, float} -> Float {startPosition, float}
  Stage1.Character {startPosition, character} -> Character {startPosition, character}
  Stage1.String {startPosition, string} -> String {startPosition, string}
  Stage1.Tupling {startPosition, count} ->
    Constructor
      { constructorPosition = startPosition,
        constructor = Constructor.tuple count
      }
  Stage1.Unit {startPosition} ->
    Constructor
      { constructorPosition = startPosition,
        constructor = Constructor.tuple 0
      }
  Stage1.Tuple {startPosition, elements} ->
    Tuple
      { startPosition,
        elements = fmap (resolve context) elements
      }
  Stage1.List {startPosition, items} ->
    List
      { startPosition,
        items = fmap (resolve context) items
      }
  Stage1.SequenceFrom {startPosition, from} ->
    let arguments = Reverse.fromList [resolve context from]
     in resolveTerm2 startPosition (Term2.Method Method.enumFrom) arguments
  Stage1.SequenceFromThen {startPosition, from, thenx} ->
    let arguments = Reverse.fromList [resolve context from, resolve context thenx]
     in resolveTerm2 startPosition (Term2.Method Method.enumFromThen) arguments
  Stage1.SequenceFromTo {startPosition, from, to} ->
    let arguments = Reverse.fromList [resolve context from, resolve context to]
     in resolveTerm2 startPosition (Term2.Method Method.enumFromTo) arguments
  Stage1.SequenceFromThenTo {startPosition, from, thenx, to} ->
    let arguments = Reverse.fromList [resolve context from, resolve context thenx, resolve context to]
     in resolveTerm2 startPosition (Term2.Method Method.enumFromThenTo) arguments
  Stage1.Comprehension {startPosition, statements} ->
    Comprehension
      { startPosition,
        statements = Statements.resolve context statements
      }
  Stage1.Record {startPosition, constructor, fields} ->
    case context != constructor of
      binding@Constructor.Binding
        { index = Constructor.Index typex constructorIndex
        } ->
          Record
            { constructorPosition = startPosition,
              constructor = Constructor.Index typex constructorIndex,
              fields = fmap (Field.resolve context binding) fields
            }
  Stage1.Update {base, updatePosition, updates} -> case first of
    Selector.Index index _ ->
      Update
        { base = resolve context base,
          updatePosition,
          updates = fmap (Select.resolve index context) updates
        }
    where
      first = case head (toList updates) of
        Stage1.Field {variable} -> context !-% variable
        Stage1.Pun {variable} -> context !-% variable
  Stage1.Let {declarations, body}
    | (context, declarations) <- Declarations.resolve context declarations ->
        Let
          { declarations,
            letBody = resolve context body
          }
  Stage1.If {condition, thenx, elsex} ->
    If
      { condition = resolve context condition,
        thenx = resolve context thenx,
        elsex = resolve context elsex
      }
  Stage1.MultiwayIf {branches} ->
    MultiwayIf
      { branches = fmap (RightHandSide.resolve context) branches
      }
  Stage1.Case {startPosition, scrutinee, cases} ->
    Case
      { startPosition,
        scrutinee = resolve context scrutinee,
        cases = fmap (Alternative.resolve context) cases
      }
  Stage1.Do {startPosition, statements} ->
    Do
      { startPosition,
        statements = Statements.resolve context statements
      }
  Stage1.Lambda {startPosition, parameters, body}
    | parameters <- toList parameters,
      initial <- head parameters,
      patterns <- tail parameters,
      parameter <- Pattern.resolve context initial ->
        Lambda
          { startPosition,
            parameter,
            body = Lambda.resolve (Pattern.augment parameter context) Lambda.Resolve {patterns} body
          }
  Stage1.LambdaCase {startPosition, cases} ->
    LambdaCase
      { startPosition,
        cases = fmap (Alternative.resolve context) cases
      }
  Stage1.Cons {startPosition} ->
    Constructor
      { constructorPosition = startPosition,
        constructor = Constructor.cons
      }
  Stage1.Infix {left, operator, right} ->
    Infix.fix $
      Infix.resolve
        context
        Stage1.Infix.Infix
          { left,
            operator,
            right
          }
  Stage1.InfixCons {head, operatorPosition, tail} ->
    Infix.fix $
      Infix.resolve
        context
        Stage1.Infix.InfixCons
          { head,
            operatorPosition,
            tail
          }
  Stage1.LeftSection {leftSection, operator} -> case operator of
    operatorPosition :@ QualifiedVariable operator -> case context !- (operatorPosition :@ operator) of
      Term.Binding
        { position,
          index,
          fixity = Fixity associativity precedence
        } -> case associativity of
          Left ->
            let left = Infix.fixWith (Just Left) precedence $ Infix.resolve context leftSection
             in resolveTerm2 position index (Nil :> left)
          _ ->
            let left = Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context leftSection
             in resolveTerm2 position index (Nil :> left)
    operatorPosition :@ QualifiedConstructor operator -> case context !=~ (operatorPosition :@ operator) of
      Constructor.Binding
        { position,
          index,
          fixity = Fixity associativity precedence
        } ->
          case associativity of
            Left ->
              let left = Infix.fixWith (Just Left) precedence $ Infix.resolve context leftSection
               in resolveConstructor2 position index (Nil :> left)
            _ ->
              let left = Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context leftSection
               in resolveConstructor2 position index (Nil :> left)
    where

  Stage1.LeftSectionCons {operatorPosition, leftSection} -> case associativity of
    Left ->
      let left = Infix.fixWith (Just Left) precedence $ Infix.resolve context leftSection
       in resolveConstructor2 operatorPosition Constructor.cons (Nil :> left)
    _ ->
      let left = Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context leftSection
       in resolveConstructor2 operatorPosition Constructor.cons (Nil :> left)
    where
      Fixity associativity precedence = Fixity Right 5
  Stage1.RightSection {operator, rightSection} -> case operator of
    operatorPosition :@ QualifiedVariable operator -> case context !- operatorPosition :@ operator of
      Term.Binding
        { index,
          fixity = Fixity associativity precedence
        } ->
          let right = case associativity of
                Right -> Infix.fixWith (Just Right) precedence $ Infix.resolve context rightSection
                _ -> Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context rightSection
           in case index of
                Term2.Index leftVariable -> RightSectionVariable {operatorPosition, leftVariable, right}
                Term2.Select leftSelector -> RightSectionSelector {operatorPosition, leftSelector, right}
                Term2.Method leftMethod -> RightSectionMethod {operatorPosition, leftMethod, right}
                Term2.RunST -> badRunSTCall operatorPosition
    operatorPosition :@ QualifiedConstructor name -> case context !=~ operatorPosition :@ name of
      Constructor.Binding
        { index,
          fixity = Fixity associativity precedence
        } ->
          let right =
                case associativity of
                  Right -> Infix.fixWith (Just Right) precedence $ Infix.resolve context rightSection
                  _ -> Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context rightSection
           in RightSectionConstructor {operatorPosition, leftConstructor = index, right}
  Stage1.RightSectionCons {operatorPosition, rightSection} -> case associativity of
    Right ->
      let right = Infix.fixWith (Just Right) precedence $ Infix.resolve context rightSection
       in RightSectionCons {operatorPosition, right}
    _ ->
      let right = Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context rightSection
       in RightSectionCons {operatorPosition, right}
    where
      Fixity associativity precedence = Fixity Right 5
  Stage1.Annotation {expression, operatorPosition, annotation} ->
    let scheme' = Scheme.resolve context annotation
        context' = Scheme.augment scheme' context
     in Annotation
          { expression = resolve context' expression,
            operatorPosition,
            annotation = scheme'
          }
