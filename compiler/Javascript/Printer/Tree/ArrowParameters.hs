module Javascript.Printer.Tree.ArrowParameters where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print, print, token)
import Javascript.Printer.Tree.FormalParameters (FormalParameters)
import Prelude hiding (print)

type ArrowParameters :: Bool -> Bool -> Type
newtype ArrowParameters yield await = ArrowParameters Lexer

instance Print (ArrowParameters yield await) where
  print (ArrowParameters ast) = ast

arrowParameters1 :: Identifier -> ArrowParameters yield await
arrowParameters1 identifier = ArrowParameters $ print identifier

arrowParameters2 :: FormalParameters yield await -> ArrowParameters yield await
arrowParameters2 parameters = ArrowParameters $ token "(" <> print parameters <> token ")"
