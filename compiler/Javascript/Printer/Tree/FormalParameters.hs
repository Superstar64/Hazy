module Javascript.Printer.Tree.FormalParameters where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.FormalParameterList (FormalParameterList)
import Prelude hiding (print)

type FormalParameters :: Bool -> Bool -> Type
newtype FormalParameters yield await = FormalParameters Lexer

instance Print (FormalParameters yield await) where
  print (FormalParameters ast) = ast

formalParameters1 :: FormalParameters yield await
formalParameters1 = FormalParameters mempty

formalParameters3 :: FormalParameterList yield await -> FormalParameters yield await
formalParameters3 ast = FormalParameters $ print ast
