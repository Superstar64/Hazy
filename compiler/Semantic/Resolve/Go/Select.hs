module Semantic.Resolve.Go.Select where

import Error (mismatchSelectors)
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context (..), (!-%), (!-*))
import qualified Semantic.Resolve.Go.CallHead as CallHead
import {-# SOURCE #-} Semantic.Resolve.Go.Expression as Expression (resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Expression (callHead_)
import Semantic.Tree.Select (Select (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.ExpressionField as Syntax (Field (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (QualifiedVariable (..), Qualifiers (..))

resolve :: Type2.Index scope -> Context scope -> Syntax.Field Position -> Select Normal Resolve scope
resolve typex context = \case
  Syntax.Field {variable, field} -> make variable (Expression.resolve context field)
  Syntax.Pun {variable = variable@(position :@ _ :- localName)} ->
    let index = context !-* position :@ Local :- localName
     in make variable $ callHead_ $ CallHead.resolveVariable position index
  where
    make name@(position :@ _) expression
      | Selector.Index typex' selector <- context !-% name,
        typex == typex' =
          Select selector expression
      | otherwise = mismatchSelectors position
