module Javascript.Printer.Tree.LexicalBinding where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print (..))
import Javascript.Printer.Tree.Initializer (Initializer)
import Prelude hiding (print)

type LexicalBinding :: Bool -> Bool -> Bool -> Type
newtype LexicalBinding inx yield await = LexicalBinding Lexer

instance Print (LexicalBinding inx yield await) where
  print (LexicalBinding ast) = ast

lexicalBinding1 :: Identifier -> Maybe (Initializer inx yield await) -> LexicalBinding inx yield await
lexicalBinding1 text initializer = LexicalBinding $ print text <> print initializer
