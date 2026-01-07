{-# LANGUAGE RoleAnnotations #-}

module Javascript.Printer.Tree.Statement where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Print)

type role Statement phantom phantom phantom

type Statement :: Bool -> Bool -> Bool -> Type
data Statement yield await return

instance Print (Statement yield await return)
