module Stage2.Check.Temporary.RightHandSide where

import Control.Monad.ST (ST)
import Stage2.Layout (Group)
import Stage2.Locality (Local)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Declaration)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.RightHandSide as Solved
import qualified Stage2.Tree.RightHandSide as Stage2 (RightHandSide (..))
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Body (Body)
import qualified Stage2.Check.Temporary.Body as Body
import {-# SOURCE #-} Stage2.Check.Temporary.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Check.Temporary.Declarations as Declarations
import qualified Stage2.Unify as Unify

data RightHandSide s scope
  = RightHandSide
      !(Body s (Scope.Declaration ':+ scope))
      !(Declarations Local s (Scope.Declaration ':+ scope))

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.RightHandSide Group Resolve scope ->
  ST s (RightHandSide s scope)
check context typex (Stage2.RightHandSide body declarations) = do
  (context, declarations) <- Declarations.check context declarations
  body <- Body.check context (shift typex) body
  pure $ RightHandSide body declarations

solve :: RightHandSide s scope -> Unify.Solve s (Solved.RightHandSide Group Check scope)
solve (RightHandSide body declarations) = do
  body <- Body.solve body
  declarations <- Declarations.solve declarations
  pure $ Solved.RightHandSide body declarations
