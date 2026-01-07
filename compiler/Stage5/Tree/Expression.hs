module Stage5.Tree.Expression where

import Control.Monad.ST (ST)
import Data.Foldable (toList)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import Stage2.Scope (Environment (..), Local)
import Stage4.Tree.Expression (Expression (..))
import Stage4.Tree.Instanciation (Instanciation (Instanciation))
import Stage5.Generate.Context (Context, fresh, localBindings, symbol, (!-))
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Mangle as Mangle
import {-# SOURCE #-} qualified Stage5.Tree.Declarations as Declarations
import qualified Stage5.Tree.Evidence as Evidence
import qualified Stage5.Tree.Statements as Statements

generate ::
  Context s scope ->
  Expression scope ->
  ST s ([Javascript.Statement 'True], Javascript.Expression)
generate context expression = do
  name <- fresh context
  let letx = Javascript.Let name
  body <- generateInto context Javascript.Variable {Javascript.name} expression
  pure (letx : body, Javascript.Variable {Javascript.name})

force :: Javascript.Expression -> Javascript.Expression
force object =
  let base =
        Javascript.Member
          { Javascript.object,
            Javascript.field = Mangle.value
          }
      value =
        Javascript.Ternary
          { Javascript.condition =
              Javascript.Member
                { Javascript.object,
                  Javascript.field = Mangle.lazy
                },
            Javascript.valid =
              Javascript.Call
                { Javascript.function = base,
                  Javascript.arguments = []
                },
            Javascript.invalid = base
          }
   in value

evaluate :: Javascript.Expression -> [Javascript.Expression] -> Javascript.Expression
evaluate thunk [] = force thunk
evaluate function arguments =
  Javascript.Call {Javascript.function, Javascript.arguments}

generateInto ::
  Context s scope ->
  Javascript.Expression ->
  Expression scope ->
  ST s [Javascript.Statement 'True]
generateInto context target = \case
  Variable {variable, instanciation = Instanciation instanciation} -> do
    name <- symbol context (context !- variable)
    let expression = Javascript.Variable {Javascript.name}
    arguments <- traverse (Evidence.generate context) (toList instanciation)
    pure [done (evaluate expression arguments)]
  Constructor {constructor = Constructor.Index {Constructor.constructorIndex}, arguments} -> do
    arguments <- traverse (thunk context) (toList arguments)
    let tag = Javascript.Number constructorIndex
        elements = map Javascript.Literal (tag : arguments)
        fields = zip Mangle.fields elements
        object =
          Javascript.Object
            { Javascript.fields
            }
    pure [done object]
  Character {character} -> do
    let string = Javascript.String {Javascript.string = Text.singleton character}
    pure [done string]
  Method
    { method = Method.Index {Method.methodIndex},
      evidence,
      instanciation = Instanciation instanciation
    } -> do
      evidence <- Evidence.generate context evidence
      name <- Context.fresh context
      let value =
            Javascript.Member
              { Javascript.object = evidence,
                Javascript.field = Mangle.fields !! methodIndex
              }
          statement = Javascript.Const name value
          expression = Javascript.Variable {Javascript.name}
      arguments <- traverse (Evidence.generate context) (toList instanciation)
      pure [statement, done (evaluate expression arguments)]
  Selector {selector = Selector.Index {Selector.selectorIndex}, argument} -> do
    (statements, object) <- generate context argument
    let select =
          Javascript.Member
            { Javascript.object,
              Javascript.field = Mangle.fields !! selectorIndex
            }
    pure $ statements ++ [done (force select)]
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    result <- generateInto context target letBody
    pure $ declarations ++ result
  Lambda {body} -> do
    name <- fresh context
    context <- pure $ localBindings (Vector.singleton name) Vector.empty context
    (statements, result) <- generate context body
    let returnx = Javascript.Return result
    pure
      [ done
          Javascript.Arrow
            { Javascript.parameters = [name],
              Javascript.body = statements ++ [returnx]
            }
      ]
  Call {function, argument} -> do
    (statements, function) <- generate context function
    argument <- thunk context argument
    let call =
          Javascript.Call
            { Javascript.function,
              Javascript.arguments = [argument]
            }
    pure $ statements ++ [done call]
  Join {statements} -> Statements.generate context target statements
  -- todo use big ints
  Integer {integer} ->
    pure
      [ done
          Javascript.Number
            { Javascript.number = fromIntegral integer
            }
      ]
  where
    done value =
      Javascript.Expression
        ( Javascript.Assign
            { Javascript.target,
              Javascript.value
            }
        )

thunk :: Context s scope -> Expression scope -> ST s Javascript.Expression
thunk context value = do
  let member =
        Javascript.Member
          { Javascript.object = Javascript.This,
            Javascript.field = Mangle.value
          }
  body <- generateInto context member value
  let flag =
        Javascript.Expression
          Javascript.Assign
            { Javascript.target =
                Javascript.Member
                  { Javascript.object = Javascript.This,
                    Javascript.field = Mangle.lazy
                  },
              Javascript.value =
                Javascript.Number
                  { Javascript.number = 0
                  }
            }
      return =
        Javascript.Return
          Javascript.Member
            { Javascript.object = Javascript.This,
              Javascript.field = Mangle.value
            }
      fields =
        [ ( Mangle.lazy,
            Javascript.Literal
              { Javascript.literal = Javascript.Number {Javascript.number = 1}
              }
          ),
          ( Mangle.value,
            Javascript.Method
              { Javascript.definition = mconcat [body, [flag], [return]]
              }
          )
        ]
  pure Javascript.Object {Javascript.fields}

declaration :: Context s scope -> Int -> Expression (Local ':+ scope) -> ST s Javascript.Expression
declaration context constraintCount expression = do
  fresh <- Vector.replicateM constraintCount (Context.fresh context)
  context <- pure $ Context.evidenceBindings fresh context
  if
    | 0 <- constraintCount -> thunk context expression
    | otherwise -> do
        (statements, result) <- generate context expression
        pure $
          Javascript.Arrow
            { Javascript.parameters = toList fresh,
              Javascript.body = statements ++ [Javascript.Return result]
            }
