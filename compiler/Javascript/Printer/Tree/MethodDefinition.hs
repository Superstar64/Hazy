module Javascript.Printer.Tree.MethodDefinition where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (print), token)
import Javascript.Printer.Tree.ClassElementName (ClassElementName)
import Javascript.Printer.Tree.FormalParameters (FormalParameters)
import {-# SOURCE #-} Javascript.Printer.Tree.StatementListItem (StatementListItem)
import Prelude hiding (print)

type MethodDefinition :: Bool -> Bool -> Type
newtype MethodDefinition yield await = MethodDefinition Lexer

instance Print (MethodDefinition yield await) where
  print (MethodDefinition ast) = ast

methodDefinition1 ::
  ClassElementName yield await ->
  FormalParameters 'False 'False ->
  [StatementListItem 'False 'False 'True] ->
  MethodDefinition yield await
methodDefinition1 name parameters body =
  MethodDefinition $
    mconcat
      [ print name,
        token "(",
        print parameters,
        token ")",
        token "{",
        print body,
        token "}"
      ]
