module Javascript.Tree.Expression
  ( Expression (..),
    Print,
    print,
  )
where

import qualified Data.List.Reverse as Reverse
import Data.Text (Text)
import qualified Javascript.Printer as Print
import qualified Javascript.Printer as Printer
import Javascript.Tree.Field (Field)
import qualified Javascript.Tree.Field as Field
import {-# SOURCE #-} Javascript.Tree.Statement (Statement)
import {-# SOURCE #-} qualified Javascript.Tree.Statement as Statement
import Prelude hiding (print)

data Expression
  = Variable {name :: !Text}
  | Object {fields :: [(Text, Field)]}
  | Number {number :: !Int}
  | BigInt {bigInt :: !Integer}
  | This
  | Member
      { object :: Expression,
        field :: !Text
      }
  | Call
      { function :: Expression,
        arguments :: [Expression]
      }
  | Assign
      { target :: Expression,
        value :: Expression
      }
  | Ternary
      { condition :: Expression,
        valid :: Expression,
        invalid :: Expression
      }
  | Arrow
      { parameters :: [Text],
        body :: [Statement 'True]
      }
  | String {string :: !Text}
  | Equal
      { left :: Expression,
        right :: Expression
      }

print :: (Print ast) => Expression -> ast
print = run

class Print ast where
  run :: Expression -> ast

instance Print (Printer.Initializer inx yield await) where
  run = Printer.initializer1 . run

instance Print (Printer.Expression inx yield await) where
  run = Printer.expression1 . run

instance Print (Printer.AssignmentExpression inx yield await) where
  run Assign {target, value} = Printer.assignmentExpression5 (run target) (run value)
  run Arrow {parameters, body}
    | parameter <- case parameters of
        [] ->
          Printer.arrowParameters2 Printer.formalParameters1
        [parameter]
          | parameter <- Printer.identifier parameter ->
              Printer.arrowParameters1 parameter
        (head : tail)
          | let binding element
                  | element <- Printer.singleNameBinding (Printer.identifier element) Nothing =
                      Printer.bindingElement1 element,
            head <- binding head,
            head <- Printer.formalParameterList1 head,
            tail <- map binding tail,
            parameters <- foldl Printer.formalParameterList2 head tail,
            parameters <- Printer.formalParameters3 parameters ->
              Printer.arrowParameters2 parameters,
      body <- Printer.conciseBody2 (Statement.print <$> body),
      function <- Printer.arrowFunction parameter body =
        Printer.assignmentExpression3 function
  run expression = Printer.assignmentExpression1 (run expression)

instance Print (Printer.ConditionalExpression inx yield await) where
  run = \case
    Ternary {condition, valid, invalid} ->
      Printer.conditionalExpression2 (run condition) (run valid) (run invalid)
    expression -> Printer.conditionalExpression1 (run expression)

instance Print (Printer.ShortCircuitExpression inx yield await) where
  run = Printer.shortCircuitExpression1 . run

instance Print (Printer.LogicalORExpression inx yield await) where
  run = Printer.logicialORExpression1 . run

instance Print (Printer.LogicalANDExpression inx yield await) where
  run = Printer.logicialANDExpression1 . run

instance Print (Printer.BitwiseORExpression inx yield await) where
  run = Printer.bitwiseORExpression1 . run

instance Print (Printer.BitwiseXORExpression inx yield await) where
  run = Printer.bitwiseXORExpression1 . run

instance Print (Printer.BitwiseANDExpression inx yield await) where
  run = Printer.bitwiseANDExpression1 . run

instance Print (Printer.EqualityExpression inx yield await) where
  run Equal {left, right} = Printer.equalityExpression4 (run left) (run right)
  run expression = Printer.equalityExpression1 (run expression)

instance Print (Printer.RelationalExpression inx yield await) where
  run = Printer.relationalExpression1 . run

instance Print (Printer.ShiftExpression yield await) where
  run = Printer.shiftExpression1 . run

instance Print (Printer.AdditivieExpression yield await) where
  run = Printer.additivieExpression1 . run

instance Print (Printer.MultiplicativeExpression yield await) where
  run = Printer.multiplicativeExpression1 . run

instance Print (Printer.ExponentiationExpression yield await) where
  run = Printer.exponentiationExpression1 . run

instance Print (Printer.UnaryExpression yield await) where
  run = Printer.unaryExpression1 . run

instance Print (Printer.UpdateExpression yield await) where
  run = Printer.updateExpression1 . run

instance Print (Printer.LeftHandSideExpression yield await) where
  run expression = case expression of
    Call {} -> Printer.leftHandSideExpression2 $ run expression
    _ -> Printer.leftHandSideExpression1 $ run expression

instance Print (Printer.CallExpression yield await) where
  run = \case
    Call {function, arguments}
      | arguments <- case arguments of
          [] -> Print.arguments1
          (head : tail)
            | head <- run head,
              head <- Print.argumentList1 head,
              tail <- run <$> tail,
              list <- foldl Print.argumentList3 head tail ->
                Print.arguments2 list ->
          case function of
            Call {} -> Print.callExpression4 (run function) arguments
            _ -> Print.callExpression1 (run function) arguments
    _ -> error "bad call expression"

instance Print (Printer.NewExpression yield await) where
  run = Printer.newExpression1 . run

instance Print (Printer.MemberExpression yield await) where
  run Member {object, field}
    | field <- Printer.identifier field =
        Printer.memberExpression3 (run object) field
  run expression = Printer.memberExpression1 (run expression)

instance Print (Printer.PrimaryExpression yield await) where
  run (Variable name)
    | name <- Printer.identifier name =
        Printer.primaryExpression2 name
  run (Object fields) = Printer.primaryExpression5 $ case fields of
    [] -> Printer.objectLiteral1
    (head : tail)
      | head <- uncurry Field.print head,
        tail <- Reverse.fromList tail,
        tail <- uncurry Field.print <$> tail,
        first <- Printer.propertyDefinitionList1 head,
        list <- foldl Printer.propertyDefinitionList2 first tail ->
          Printer.objectLiteral2 list
  run Number {number}
    | number <- Printer.int number,
      literal <- Printer.literal3 number =
        Printer.primaryExpression3 literal
  run BigInt {bigInt}
    | number <- Printer.bigInt bigInt,
      literal <- Printer.literal3 number =
        Printer.primaryExpression3 literal
  run This = Printer.primaryExpression1
  run String {string}
    | string <- Printer.string string,
      literal <- Printer.literal4 string =
        Printer.primaryExpression3 literal
  run expression = Printer.primaryExpression13 (run expression)
