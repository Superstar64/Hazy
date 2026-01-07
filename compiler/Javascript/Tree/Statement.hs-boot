{-# LANGUAGE RoleAnnotations #-}

module Javascript.Tree.Statement where

import Data.Kind (Type)
import qualified Javascript.Printer as Printer

type role Statement nominal

type Statement :: Bool -> Type
data Statement local

print :: Statement 'True -> Printer.StatementListItem yield await 'True
