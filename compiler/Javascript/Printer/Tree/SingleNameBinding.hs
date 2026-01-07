module Javascript.Printer.Tree.SingleNameBinding where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print (..))
import Javascript.Printer.Tree.Initializer (Initializer)
import Prelude hiding (print)

type SingleNameBinding :: Bool -> Bool -> Type
newtype SingleNameBinding yield await = SingleNameBinding Lexer

instance Print (SingleNameBinding yield await) where
  print (SingleNameBinding ast) = ast

singleNameBinding :: Identifier -> Maybe (Initializer 'True yield await) -> SingleNameBinding yield await
singleNameBinding identifier initializer = SingleNameBinding $ print identifier <> print initializer
