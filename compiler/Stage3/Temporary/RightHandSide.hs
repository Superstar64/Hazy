module Stage3.Temporary.RightHandSide where

import Control.Monad.ST (ST)
import Stage2.Layout (Normal)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.RightHandSide as Solved
import qualified Stage2.Tree.RightHandSide as Stage2 (RightHandSide (..))
import Stage3.Check.Context (Context)
import Stage3.Temporary.Body (Body)
import qualified Stage3.Temporary.Body as Body
import {-# SOURCE #-} Stage3.Temporary.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage3.Temporary.Declarations as Declarations
import qualified Stage3.Unify as Unify

data RightHandSide s scope
  = RightHandSide
      !(Body s (Scope.Declaration ':+ scope))
      !(Declarations Local s (Scope.Declaration ':+ scope))

instance Unify.Zonk RightHandSide where
  zonk zonker (RightHandSide body declarations) = do
    body <- Unify.zonk zonker body
    declarations <- Unify.zonk zonker declarations
    pure $ RightHandSide body declarations

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.RightHandSide Normal Resolve scope ->
  ST s (RightHandSide s scope)
check context typex (Stage2.RightHandSide body declarations) = do
  (context, declarations) <- Declarations.check context declarations
  body <- Body.check context (shift typex) body
  pure $ RightHandSide body declarations

solve :: RightHandSide s scope -> ST s (Solved.RightHandSide Normal Check scope)
solve (RightHandSide body declarations) = do
  body <- Body.solve body
  declarations <- Declarations.solve declarations
  pure $ Solved.RightHandSide body declarations
