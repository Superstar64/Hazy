{-# LANGUAGE RoleAnnotations #-}

module Javascript.Printer.Tree.StatementListItem where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Print)

type role StatementListItem phantom phantom phantom

type StatementListItem :: Bool -> Bool -> Bool -> Type
data StatementListItem yield await return

instance Print (StatementListItem yield await return)
