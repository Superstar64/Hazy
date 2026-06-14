{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Expression where

import Data.Foldable (toList)
import Data.List.Reverse (List (..))
import qualified Data.List.Reverse as Reverse
import qualified Semantic.Index.Constructor as Constructor (Index (..), cons)
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term2 as Term2
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Semantic.Resolve.Binding.Term as Term (Binding (..))
import Semantic.Resolve.Context
  ( Context (..),
    (!-),
    (!-%),
    (!-*),
    (!=),
    (!=*~),
    (!=~),
  )
import qualified Semantic.Resolve.Go.Alternative as Alternative (resolve)
import qualified Semantic.Resolve.Go.CallHead as CallHead
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Declarations as Declarations
import qualified Semantic.Resolve.Go.ExpressionField as Field (resolve)
import qualified Semantic.Resolve.Go.Lambda as Lambda (resolve)
import qualified Semantic.Resolve.Go.Pattern as Pattern (augment, resolve)
import qualified Semantic.Resolve.Go.RightHandSide as RightHandSide (resolve)
import qualified Semantic.Resolve.Go.Scheme as Scheme (augment, resolve)
import qualified Semantic.Resolve.Go.Select as Select (resolve)
import qualified Semantic.Resolve.Go.Statements as Statements (resolve)
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.ExpressionInfix as Infix (fix, fixWith, resolve)
import Semantic.Stage (Equal (..), Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Expression (Explicit (..), Expression (..))
import Syntax.Extensions (permissiveUpdates)
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import qualified Syntax.Tree.Expression as Syntax (Expression (..))
import qualified Syntax.Tree.ExpressionField as Syntax (Field (Field, Pun))
import qualified Syntax.Tree.ExpressionField as Syntax.Field
import qualified Syntax.Tree.ExpressionInfix as Syntax.Infix
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (QualifiedName (..))
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

resolve :: Context scope -> Syntax.Expression Position -> Expression Normal Resolve scope
resolve context expression = resolveWith context expression []

resolveWith ::
  Context scope ->
  Syntax.Expression Position ->
  [Expression Normal Resolve scope] ->
  Expression Normal Resolve scope
resolveWith context Syntax.Variable {variable = startPosition :@ variable} arguments =
  resolveTerm2 startPosition (context !-* startPosition :@ variable) (Reverse.fromList arguments)
resolveWith context Syntax.Constructor {constructor = startPosition :@ constructor} arguments =
  resolveConstructor2 startPosition (context !=*~ startPosition :@ constructor) (Reverse.fromList arguments)
resolveWith context Syntax.Call {function, argument} arguments
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
  Syntax.Integer {startPosition, integer} ->
    Integer
      { startPosition,
        integer,
        evidence = Inferred
      }
  Syntax.Float {startPosition, float} ->
    Float
      { startPosition,
        float,
        evidence = Inferred
      }
  Syntax.Character {startPosition, character} -> Character {startPosition, character}
  Syntax.String {startPosition, string} -> String {startPosition, string}
  Syntax.Tupling {startPosition, count} ->
    CallHead
      { callHead = CallHead.resolveTupling startPosition (count + 1)
      }
  Syntax.Unit {startPosition} ->
    CallHead
      { callHead = CallHead.resolveTupling startPosition 0
      }
  Syntax.Tuple {startPosition, elements} ->
    Tuple
      { startPosition,
        elements = fmap (resolve context) elements
      }
  Syntax.List {startPosition, items} ->
    List
      { startPosition,
        items = fmap (resolve context) items
      }
  Syntax.SequenceFrom {startPosition, from} ->
    let arguments = Reverse.fromList [resolve context from]
     in resolveTerm2 startPosition (Term2.Method Method.enumFrom) arguments
  Syntax.SequenceFromThen {startPosition, from, thenx} ->
    let arguments = Reverse.fromList [resolve context from, resolve context thenx]
     in resolveTerm2 startPosition (Term2.Method Method.enumFromThen) arguments
  Syntax.SequenceFromTo {startPosition, from, to} ->
    let arguments = Reverse.fromList [resolve context from, resolve context to]
     in resolveTerm2 startPosition (Term2.Method Method.enumFromTo) arguments
  Syntax.SequenceFromThenTo {startPosition, from, thenx, to} ->
    let arguments = Reverse.fromList [resolve context from, resolve context thenx, resolve context to]
     in resolveTerm2 startPosition (Term2.Method Method.enumFromThenTo) arguments
  Syntax.Comprehension {startPosition, statements} ->
    Comprehension
      { startPosition,
        statements = Statements.resolve context statements
      }
  Syntax.Record {startPosition, constructor, fields} ->
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
  Syntax.Update {base, updatePosition, updates} -> case first of
    Selector.Index index _ ->
      Update
        { base = resolve context base,
          updatePosition,
          updateType = index,
          updates = fmap (Select.resolve index context) updates,
          updateInfo = Inferred,
          permissive = permissiveUpdates $ extensions context
        }
    where
      first = case head (toList updates) of
        Syntax.Field {variable} -> context !-% variable
        Syntax.Pun {variable} -> context !-% variable
  Syntax.Let {declarations, body}
    | (context, declarations) <- Declarations.resolve context declarations ->
        Let
          { declarations,
            letBody = resolve context body
          }
  Syntax.If {condition, thenx, elsex} ->
    If
      { condition = resolve context condition,
        thenx = resolve context thenx,
        elsex = resolve context elsex
      }
  Syntax.MultiwayIf {branches} ->
    MultiwayIf
      { branches = fmap (RightHandSide.resolve context) branches
      }
  Syntax.Case {startPosition, scrutinee, cases} ->
    Case
      { startPosition,
        scrutinee = resolve context scrutinee,
        cases = fmap (Alternative.resolve context) cases
      }
  Syntax.Do {startPosition, statements} ->
    Do
      { startPosition,
        dox = Statements.resolve context statements
      }
  Syntax.Lambda {startPosition, parameters, body}
    | parameters <- toList parameters,
      initial <- head parameters,
      patterns <- tail parameters,
      parameter <- Pattern.resolve context initial ->
        Lambda
          { startPosition,
            parameter,
            body = Lambda.resolve (Pattern.augment parameter context) patterns body
          }
  Syntax.LambdaCase {startPosition, cases} ->
    LambdaCase
      { startPosition,
        cases = fmap (Alternative.resolve context) cases
      }
  Syntax.Cons {startPosition} ->
    CallHead
      { callHead = CallHead.resolveCons startPosition
      }
  Syntax.Infix {left, operator, right} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.Infix
          { left,
            operator,
            right
          }
  Syntax.InfixCons {head, operatorPosition, tail} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.InfixCons
          { head,
            operatorPosition,
            tail
          }
  Syntax.Negate {startPosition, negative} ->
    Infix.fix $
      Infix.resolve
        context
        Syntax.Infix.Negate
          { startPosition,
            negative
          }
  Syntax.LeftSection {leftSection, operator = operatorPosition :@ operator} -> case operator of
    QualifiedVariable operator -> resolveTerm2 position index (Nil :> left)
      where
        Term.Binding {position, index, fixity} = context !- operatorPosition :@ operator
        left = section Left fixity leftSection
    QualifiedConstructor operator -> resolveConstructor2 position index (Nil :> left)
      where
        Constructor.Binding {position, index, fixity} = context !=~ operatorPosition :@ operator
        left = section Left fixity leftSection
  Syntax.LeftSectionCons {operatorPosition, leftSection} ->
    resolveConstructor2 operatorPosition Constructor.cons (Nil :> left)
    where
      fixity = Fixity {associativity = Right, precedence = 5}
      left = section Left fixity leftSection
  Syntax.RightSection {operator = operatorPosition :@ operator, rightSection} -> case operator of
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
  Syntax.RightSectionCons {operatorPosition, rightSection} ->
    RightSection {operatorPosition, left = CallHead.resolveCons operatorPosition, right}
    where
      right = section Right fixity rightSection
      fixity = Fixity {associativity = Right, precedence = 5}
  Syntax.Annotation {expression, operatorPosition, annotation} ->
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
