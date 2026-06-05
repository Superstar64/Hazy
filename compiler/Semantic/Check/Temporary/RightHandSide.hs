module Semantic.Check.Temporary.RightHandSide where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Body (Body)
import qualified Semantic.Check.Temporary.Body as Body
import {-# SOURCE #-} Semantic.Check.Temporary.Declarations (Declarations)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Declarations as Declarations
import Semantic.Layout (Group)
import Semantic.Locality (Local)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope (Declaration)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.RightHandSide as Semantic (RightHandSide (..))
import qualified Semantic.Tree.RightHandSide as Solved
import qualified Semantic.Unify as Unify

data RightHandSide s scope
  = RightHandSide
      !(Body s (Scope.Declaration ':+ scope))
      !(Declarations Local s (Scope.Declaration ':+ scope))

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.RightHandSide Group Resolve scope ->
  ST s (RightHandSide s scope)
check context typex (Semantic.RightHandSide body declarations) = do
  (context, declarations) <- Declarations.check context declarations
  body <- Body.check context (shift typex) body
  pure $ RightHandSide body declarations

solve :: RightHandSide s scope -> Unify.Solve s (Solved.RightHandSide Group Check scope)
solve (RightHandSide body declarations) = do
  body <- Body.solve body
  declarations <- Declarations.solve declarations
  pure $ Solved.RightHandSide body declarations
