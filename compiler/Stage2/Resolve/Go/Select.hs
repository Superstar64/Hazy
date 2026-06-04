module Stage2.Resolve.Go.Select where

import Error (mismatchSelectors)
import Stage1.Position (Position)
import qualified Stage1.Tree.ExpressionField as Stage1 (Field (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (QualifiedVariable (..), Qualifiers (..))
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context (..), (!-%), (!-*))
import qualified Stage2.Resolve.Go.CallHead as CallHead
import {-# SOURCE #-} Stage2.Resolve.Go.Expression as Expression (resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Expression (callHead_)
import Stage2.Tree.Select (Select (..))

resolve :: Type2.Index scope -> Context scope -> Stage1.Field Position -> Select Normal Resolve scope
resolve typex context = \case
  Stage1.Field {variable, field} -> make variable (Expression.resolve context field)
  Stage1.Pun {variable = variable@(position :@ _ :- localName)} ->
    let index = context !-* position :@ Local :- localName
     in make variable $ callHead_ $ CallHead.resolveVariable position index
  where
    make name@(position :@ _) expression
      | Selector.Index typex' selector <- context !-% name,
        typex == typex' =
          Select selector expression
      | otherwise = mismatchSelectors position
