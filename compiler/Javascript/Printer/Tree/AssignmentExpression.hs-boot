{-# LANGUAGE RoleAnnotations #-}

module Javascript.Printer.Tree.AssignmentExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Print)

type role AssignmentExpression phantom phantom phantom

type AssignmentExpression :: Bool -> Bool -> Bool -> Type
data AssignmentExpression a b c

instance Print (AssignmentExpression a b c)
