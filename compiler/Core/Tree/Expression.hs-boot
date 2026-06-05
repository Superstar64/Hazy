{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Expression where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (Evidence)
import {-# SOURCE #-} Core.Tree.Statements (Statements)
import Data.Kind (Type)
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Definition as Semantic
import {-# SOURCE #-} qualified Semantic.Tree.Expression as Semantic

type role Expression nominal

type Expression :: Environment -> Type
data Expression scope

data Direction = Construct | Destruct

instance Shift Expression

instance Shift.Functor Expression

instance Shift2.Functor Expression

instance Substitute.Functor Expression

instance Show (Expression scope)

instance Scope.Show Expression

monoVariable :: Term.Index scope -> Expression scope
lambdaVariable :: Expression (Scope.SimpleDeclaration ':+ scopes)
patternVariable :: Expression (Scope.Pattern ':+ scopes)
patternVariableAt :: Int -> Expression (Scope.SimplePattern ':+ scope)
character_ :: Char -> Expression scope
join_ :: Statements scope -> Expression scope
newtype_ :: Constructor.Index scope -> Expression scope -> Direction -> Expression scope
eq :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
eqChar :: Expression scope -> Expression scope -> Expression scope
integer_ :: Integer -> Evidence scope -> Expression scope
float_ :: Rational -> Evidence scope -> Expression scope
call :: Expression scope -> Expression scope -> Expression scope

class Simplify source where
  simplify :: source Normal Check scope -> Expression scope

instance Simplify Semantic.Expression

instance Simplify Semantic.Definition
