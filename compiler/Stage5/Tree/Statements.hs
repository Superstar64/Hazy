module Stage5.Tree.Statements (generate) where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Stage2.Index.Constructor as Constructor
import Stage4.Tree.Pattern (Pattern (..))
import Stage4.Tree.Statements (Statements (..))
import Stage5.Generate.Context (Context (..), fresh, localBindings)
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Mangle
import {-# SOURCE #-} qualified Stage5.Tree.Declarations as Declarations
import {-# SOURCE #-} qualified Stage5.Tree.Expression as Expression

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
  Bind {patternx, check, thenx} -> do
    (prelude, check) <- Expression.generate context check
    case patternx of
      Constructor {constructor = Constructor.Index {constructorIndex}, patterns} -> do
        names <- Vector.replicateM patterns (Context.fresh context)
        context <- pure $ Context.patternBindings names context
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
      Character {character} -> do
        let names = Vector.empty
        context <- pure $ Context.patternBindings names context
        thenx <- attempt context target label thenx
        let ifx = Javascript.If condition thenx
            condition =
              Javascript.Equal
                { left = check,
                  right =
                    Javascript.String
                      { string = Text.singleton character
                      }
                }
        pure $ prelude ++ [ifx]
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    result <- attempt context target label letBody
    pure $ declarations ++ result
  LetOne {declaration, body} -> do
    thunk <- Expression.thunk context declaration
    name <- fresh context
    let declaration = Javascript.Const name thunk
    body <- attempt (localBindings (Vector.singleton name) Vector.empty context) target label body
    pure $ declaration : body
  Branch {left, right} -> do
    left <- attempt context target label left
    right <- attempt context target label right
    pure $ left ++ right
  Bottom -> pure []
