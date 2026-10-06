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
    Eq (..),
    shows,
    Vacuous,
    Singleton (..),
  )
where

import Data.Kind (Constraint, Type)
import Prelude hiding (Eq (..), Show, shows, showsPrec)

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

type Eq :: (Environment -> Type) -> Constraint
class Eq typex where
  (==) :: typex scope -> typex scope -> Bool
  infix 4 ==

type Vacuous :: Environment -> Type
data Vacuous scope

instance Show Vacuous where
  showsPrec _ = \case {}

instance Eq Vacuous where
  (==) = \case {}

type Singleton :: Environment -> Type
data Singleton scope = Singleton

instance Show Singleton where
  showsPrec _ Singleton = showString "Singleton"

instance Eq Singleton where
  Singleton == Singleton = True
