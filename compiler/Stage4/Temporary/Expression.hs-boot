{-# LANGUAGE RoleAnnotations #-}

module Stage4.Temporary.Expression where

import Data.Kind (Type)
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Expression as Stage3
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Real

type role Expression nominal

type Expression :: Environment -> Type
data Expression scope

instance Shift Expression

instance Shift.Functor Expression

instance Term.Functor Expression

instance Show (Expression scope)

monoVariable :: Term.Index scope -> Expression scope
lambdaVariable :: Expression (Scope.Declaration ':+ scopes)
call :: Expression scope -> Expression scope -> Expression scope
simplify :: Stage3.Expression scope -> Expression scope
finish :: Expression scope -> Real.Expression scope
