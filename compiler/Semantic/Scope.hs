module Semantic.Scope
  ( Environment ((:+)),
    Scope,
    Local,
    Declaration,
    Pattern,
    GroupTerm,
    GroupType,
    SimplePattern,
    SimpleDeclaration,
    Global,
    Show (..),
    shows,
  )
where

import Data.Kind (Constraint, Type)
import Prelude hiding (Show, shows, showsPrec)

data Environment
  = Scope :+ Environment
  | Global

type Global = 'Global

infixr 5 :+

data Scope
  = Local
  | Declaration
  | Pattern
  | GroupTerm
  | GroupType
  | SimplePattern
  | SimpleDeclaration

type Local = 'Local

type Declaration = 'Declaration

type Pattern = 'Pattern

type GroupTerm = 'GroupTerm

type GroupType = 'GroupType

type SimplePattern = 'SimplePattern

type SimpleDeclaration = 'SimpleDeclaration

type Show :: (Environment -> Type) -> Constraint
class Show typex where
  showsPrec :: Int -> typex scope -> ShowS

shows :: (Show typex) => typex scope -> ShowS
shows = showsPrec 0
