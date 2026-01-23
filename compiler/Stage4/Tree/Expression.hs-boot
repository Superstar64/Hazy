{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Expression where

import Data.Kind (Type)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Definition as Stage3
import {-# SOURCE #-} qualified Stage3.Tree.Expression as Stage3
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2

type role Expression nominal

type Expression :: Environment -> Type
data Expression scope

instance Shift Expression

instance Shift.Functor Expression

instance Shift2.Functor Expression

instance Show (Expression scope)

monoVariable :: Term.Index scope -> Expression scope
lambdaVariable :: Expression (Scope.Declaration ':+ scopes)
call :: Expression scope -> Expression scope -> Expression scope

class Simplify source where
  simplify :: source scope -> Expression scope

instance Simplify Stage3.Expression

instance Simplify Stage3.Definition
