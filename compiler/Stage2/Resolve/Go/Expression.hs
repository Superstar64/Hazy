{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Expression where

import Data.Foldable (toList)
import Data.List.Reverse (List (..))
import qualified Data.List.Reverse as Reverse
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import qualified Stage1.Tree.Expression as Stage1 (Expression (..))
import qualified Stage1.Tree.ExpressionField as Stage1 (Field (Field, Pun))
import qualified Stage1.Tree.ExpressionField as Stage1.Field
import qualified Stage1.Tree.ExpressionInfix as Stage1.Infix
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (QualifiedName (..))
import qualified Stage2.Index.Constructor as Constructor (Index (..), cons)
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term2 as Term2
import Stage2.Layout (Normal)
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
import qualified Stage2.Resolve.Go.Alternative as Alternative (resolve)
import qualified Stage2.Resolve.Go.CallHead as CallHead
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Declarations as Declarations
import qualified Stage2.Resolve.Go.ExpressionField as Field (resolve)
import qualified Stage2.Resolve.Go.Lambda as Lambda (resolve)
import qualified Stage2.Resolve.Go.Pattern as Pattern (augment, resolve)
import qualified Stage2.Resolve.Go.RightHandSide as RightHandSide (resolve)
import qualified Stage2.Resolve.Go.Scheme as Scheme (augment, resolve)
import qualified Stage2.Resolve.Go.Select as Select (resolve)
import qualified Stage2.Resolve.Go.Statements as Statements (resolve)
import Stage2.Stage (Equal (..), Resolve)
import {-# SOURCE #-} qualified Stage2.Resolve.Temporary.ExpressionInfix as Infix (fix, fixWith, resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.Expression (Explicit (..), Expression (..))
import Prelude hiding (Bool (False, True), Either (Left, Right))

resolveTerm2 ::
  Position ->
  Term2.Index scope ->
  Reverse.List (Expression Normal Resolve scope) ->
  Expression Normal Resolve scope
resolveTerm2 position index Nil = CallHead {callHead = CallHead.resolveVariable position index}
resolveTerm2 startPosition Term2.RunST (Nil :> imperative) =
  RunST
    { startPosition,
      imperative,
      unsupported = Refl
    }
resolveTerm2 position index (arguments :> argument) =
  Call
    { function = resolveTerm2 position index arguments,
      argument
    }

resolveConstructor2 ::
  Position ->
  Constructor.Index scope ->
  Reverse.List (Expression Normal Resolve scope) ->
  Expression Normal Resolve scope
resolveConstructor2 position constructor Nil =
  CallHead {callHead = CallHead.resolveConstructor position constructor}
resolveConstructor2 position index (arguments :> argument) =
  Call
    { function = resolveConstructor2 position index arguments,
      argument
    }

resolve :: Context scope -> Stage1.Expression Position -> Expression Normal Resolve scope
resolve context expression = resolveWith context expression []

resolveWith ::
  Context scope ->
  Stage1.Expression Position ->
  [Expression Normal Resolve scope] ->
  Expression Normal Resolve scope
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
  Stage1.Integer {startPosition, integer} ->
    Integer
      { startPosition,
        integer,
        evidence = Inferred
      }
  Stage1.Float {startPosition, float} ->
    Float
      { startPosition,
        float,
        evidence = Inferred
      }
  Stage1.Character {startPosition, character} -> Character {startPosition, character}
  Stage1.String {startPosition, string} -> String {startPosition, string}
  Stage1.Tupling {startPosition, count} ->
    CallHead
      { callHead = CallHead.resolveTupling startPosition (count + 1)
      }
  Stage1.Unit {startPosition} ->
    CallHead
      { callHead = CallHead.resolveTupling startPosition 0
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
        statements = Statements.resolve context statements,
        unsupported = Refl
      }
  Stage1.Record {startPosition, constructor, fields} ->
    case context != constructor of
      binding@Constructor.Binding
        { index = Constructor.Index typex constructorIndex
        } ->
          Record
            { constructorPosition = startPosition,
              constructor = Constructor.Index typex constructorIndex,
              constructorInfo = Inferred,
              fields = fmap (Field.resolve context binding) fields
            }
  Stage1.Update {base, updatePosition, updates} -> case first of
    Selector.Index index _ ->
      Update
        { base = resolve context base,
          updatePosition,
          updates = fmap (Select.resolve index context) updates,
          unsupported = Refl
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
        dox = Statements.resolve context statements
      }
  Stage1.Lambda {startPosition, parameters, body}
    | parameters <- toList parameters,
      initial <- head parameters,
      patterns <- tail parameters,
      parameter <- Pattern.resolve context initial ->
        Lambda
          { startPosition,
            parameter,
            body = Lambda.resolve (Pattern.augment parameter context) patterns body
          }
  Stage1.LambdaCase {startPosition, cases} ->
    LambdaCase
      { startPosition,
        cases = fmap (Alternative.resolve context) cases
      }
  Stage1.Cons {startPosition} ->
    CallHead
      { callHead = CallHead.resolveCons startPosition
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
  Stage1.LeftSection {leftSection, operator = operatorPosition :@ operator} -> case operator of
    QualifiedVariable operator -> resolveTerm2 position index (Nil :> left)
      where
        Term.Binding {position, index, fixity} = context !- operatorPosition :@ operator
        left = section Left fixity leftSection
    QualifiedConstructor operator -> resolveConstructor2 position index (Nil :> left)
      where
        Constructor.Binding {position, index, fixity} = context !=~ operatorPosition :@ operator
        left = section Left fixity leftSection
  Stage1.LeftSectionCons {operatorPosition, leftSection} ->
    resolveConstructor2 operatorPosition Constructor.cons (Nil :> left)
    where
      fixity = Fixity {associativity = Right, precedence = 5}
      left = section Left fixity leftSection
  Stage1.RightSection {operator = operatorPosition :@ operator, rightSection} -> case operator of
    QualifiedVariable operator -> RightSection {operatorPosition, left, right}
      where
        Term.Binding {index, fixity} = context !- operatorPosition :@ operator
        left = CallHead.resolveVariable operatorPosition index
        right = section Right fixity rightSection
    QualifiedConstructor name -> RightSection {operatorPosition, left, right}
      where
        Constructor.Binding {index, fixity} = context !=~ operatorPosition :@ name
        left = CallHead.resolveConstructor operatorPosition index
        right = section Right fixity rightSection
  Stage1.RightSectionCons {operatorPosition, rightSection} ->
    RightSection {operatorPosition, left = CallHead.resolveCons operatorPosition, right}
    where
      right = section Right fixity rightSection
      fixity = Fixity {associativity = Right, precedence = 5}
  Stage1.Annotation {expression, operatorPosition, annotation} ->
    let scheme' = Scheme.resolve context annotation
        context' = Scheme.augment scheme' context
     in Annotation
          { expression = Explicit $ resolve context' expression,
            operatorPosition,
            annotation = scheme',
            instanciation = Inferred
          }
  where
    section wanted Fixity {associativity, precedence} section
      | wanted == associativity =
          Infix.fixWith (Just associativity) precedence $ Infix.resolve context section
      | otherwise =
          Infix.fixWith Nothing (precedence + 1) $ Infix.resolve context section
