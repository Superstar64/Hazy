module Generate.Go.Statements (generate) where

import Control.Monad.ST (ST)
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import Core.Tree.Statements (Statements (..))
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict.Vector
import Generate.Context (Context (..), fresh, singleBinding)
import qualified Generate.Context as Context
import {-# SOURCE #-} qualified Generate.Go.Declarations as Declarations
import {-# SOURCE #-} qualified Generate.Go.Expression as Expression
import qualified Generate.Mangle as Mangle
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Semantic.Index.Constructor as Constructor

generate ::
  Context s scope ->
  Javascript.Expression ->
  Statements scope ->
  ST s [Javascript.Statement 'True]
generate context@Context {builtin = Mangle.Builtin {abort}} target statements = do
  label <- Context.fresh context
  statements <- attempt context target label statements
  let bottom =
        Javascript.Expression
          Javascript.Call
            { function = Javascript.Variable {name = abort},
              arguments = []
            }
  pure [Javascript.Label label (statements ++ [bottom])]

attempt ::
  Context s scope ->
  Javascript.Expression ->
  Text ->
  Statements scope ->
  ST s [Javascript.Statement 'True]
attempt context target label = \case
  Done {done} -> do
    assign <- Expression.generateInto context target done
    let break = Javascript.Break label
    pure $ assign ++ [break]
  Bind
    { constructor = Constructor.Index {constructorIndex},
      constructorInfo = constructorInfo@ConstructorInfo {entries},
      check,
      thenx
    } -> do
      (prelude, check) <- Expression.generate context check
      names <- Strict.Vector.replicateM (length entries) (Context.fresh context)
      context <- pure $ Context.patternBindings names constructorInfo context
      thenx <- attempt context target label thenx
      let ifx = Javascript.If condition (bind ++ thenx)
          condition =
            Javascript.Equal
              { left =
                  Javascript.Member
                    { object = check,
                      field = head Mangle.fields
                    },
                right =
                  Javascript.Number
                    { number = constructorIndex
                    }
              }
          bind = do
            (field, name) <- zip (tail Mangle.fields) (toList names)
            let member =
                  Javascript.Member
                    { object = check,
                      field
                    }
            pure $ Javascript.Const name member
      pure $ prelude ++ [ifx]
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    result <- attempt context target label letBody
    pure $ declarations ++ result
  LetOne {declaration, body} -> do
    thunk <- Expression.thunk context Expression.Done declaration
    name <- fresh context
    let declaration = Javascript.Const name thunk
    body <- attempt (singleBinding name context) target label body
    pure $ declaration : body
  Branch {left, right} -> do
    left <- attempt context target label left
    right <- attempt context target label right
    pure $ left ++ right
  Bottom -> pure []
