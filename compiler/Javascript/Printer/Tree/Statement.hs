module Javascript.Printer.Tree.Statement where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.Block (Block)
import Javascript.Printer.Tree.BreakStatement (BreakStatement)
import Javascript.Printer.Tree.Expression (Expression)
import Javascript.Printer.Tree.IfStatement (IfStatement)
import Javascript.Printer.Tree.LabelledStatement (LabelledStatement)
import Javascript.Printer.Tree.ReturnStatement (ReturnStatement)
import Prelude hiding (print)

type Statement :: Bool -> Bool -> Bool -> Type
newtype Statement yield await return = Statement Lexer

instance Print (Statement yield await return) where
  print (Statement ast) = ast

statement1 :: Block yield await return -> Statement yield await return
statement1 ast = Statement $ print ast

-- todo deal with lookahead requirement
statement4 :: Expression 'True yield await -> Statement yield await returnx
statement4 expression = Statement $ print expression <> token ";"

statement5 :: IfStatement yield await return -> Statement yield await return
statement5 ifstatement = Statement $ print ifstatement

statement8 :: BreakStatement yield await -> Statement yield await return
statement8 breakstatement = Statement $ print breakstatement

statement9 :: ReturnStatement yield await -> Statement yield await 'True
statement9 ast = Statement $ print ast

statement11 :: LabelledStatement yield await return -> Statement yield await return
statement11 ast = Statement $ print ast
