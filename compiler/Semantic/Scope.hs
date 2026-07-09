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
    IsVacuous (..),
    Equal (..),
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

class IsVacuous functor where
  isVacuous :: Equal Vacuous functor

instance IsVacuous Vacuous where
  isVacuous = Refl

type Equal :: (Environment -> Type) -> (Environment -> Type) -> Type
data Equal functor functor' where
  Refl :: Equal functor functor
