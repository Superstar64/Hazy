module Stage5.Tree.Expression where

import Control.Monad (zipWithM)
import Control.Monad.ST (ST)
import Data.Char (ord)
import Data.Foldable (toList)
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import Stage4.Tree.ConstructorInfo (ConstructorInfo (..))
import Stage4.Tree.EntryInfo (EntryInfo (..))
import Stage4.Tree.Expression (Expression (..))
import Stage4.Tree.Instanciation (Instanciation (Instanciation))
import Stage4.Tree.MethodInfo (MethodInfo (..))
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver
import qualified Stage5.Generate.Binding.Term as Term
import Stage5.Generate.Context (Context (..), fresh, singleBinding, symbol, (!-))
import qualified Stage5.Generate.Context as Context
import qualified Stage5.Generate.Info as Info
import qualified Stage5.Generate.Mangle as Mangle
import {-# SOURCE #-} qualified Stage5.Tree.Declarations as Declarations
import qualified Stage5.Tree.Evidence as Evidence
import qualified Stage5.Tree.Hook as Hook
import qualified Stage5.Tree.Statements as Statements

delay :: [Javascript.Statement 'True] -> Javascript.Expression
delay body =
  let flag =
        Javascript.Expression
          Javascript.Assign
            { target =
                Javascript.Member
                  { object = Javascript.This,
                    field = Mangle.lazy
                  },
              value =
                Javascript.Number
                  { number = 0
                  }
            }
      return =
        Javascript.Return
          Javascript.Member
            { object = Javascript.This,
              field = Mangle.value
            }
      fields =
        [ ( Mangle.lazy,
            Javascript.Literal
              { literal = Javascript.Number {number = 1}
              }
          ),
          ( Mangle.value,
            Javascript.Method
              { definition = mconcat [body, [flag], [return]]
              }
          )
        ]
   in Javascript.Object {fields}

done :: Javascript.Expression
done =
  Javascript.Member
    { object = Javascript.This,
      field = Mangle.value
    }

force :: Javascript.Expression -> Javascript.Expression
force object =
  let base =
        Javascript.Member
          { object,
            field = Mangle.value
          }
      value =
        Javascript.Ternary
          { condition =
              Javascript.Member
                { object,
                  field = Mangle.lazy
                },
            valid =
              Javascript.Call
                { function = base,
                  arguments = []
                },
            invalid = base
          }
   in value

evaluate :: Bool -> Javascript.Expression -> [Javascript.Expression] -> Javascript.Expression
evaluate False thunk [] = force thunk
evaluate True value [] = value
evaluate _ function arguments =
  Javascript.Call {function, arguments}

generate ::
  Context s scope ->
  Expression scope ->
  ST s ([Javascript.Statement 'True], Javascript.Expression)
generate context expression = do
  name <- fresh context
  let letx = Javascript.Let name
  body <- generateInto context Javascript.Variable {name} expression
  pure (letx : body, Javascript.Variable {name})

generateInto ::
  Context s scope ->
  Javascript.Expression ->
  Expression scope ->
  ST s [Javascript.Statement 'True]
generateInto context target = \case
  Variable {variable, instanciation = Instanciation instanciation} -> do
    let Term.Binding {name, strict} = context !- variable
    name <- symbol context name
    let expression = Javascript.Variable {name}
    arguments <- traverse (Evidence.generate context) (toList instanciation)
    pure [done (evaluate strict expression arguments)]
  Constructor
    { constructor = Constructor.Index {constructorIndex},
      arguments,
      constructorInfo = ConstructorInfo {entries}
    } -> do
      let process EntryInfo {strict} argument =
            if Info.strict strict
              then do
                (extra, value) <- generate context argument
                pure (extra, value)
              else do
                value <- thunk context Done argument
                pure ([], value)
      (extra, arguments) <- unzip <$> zipWithM process (toList entries) (toList arguments)
      let tag = Javascript.Number constructorIndex
          elements = map Javascript.Literal (tag : arguments)
          fields = zip Mangle.fields elements
          object = Javascript.Object {fields}
      pure $ concat extra ++ [done object]
  Character {character} -> do
    let string = Javascript.Number {number = ord character}
    pure [done string]
  Method
    { method = Method.Index {methodIndex},
      evidence,
      instanciation = Instanciation instanciation,
      methodInfo = MethodInfo {constraintCount}
    } -> do
      evidence <- Evidence.generate context evidence
      name <- Context.fresh context
      let value =
            Javascript.Member
              { object = evidence,
                field = Mangle.fields !! (methodIndex + constraintCount)
              }
          statement = Javascript.Const name value
          expression = Javascript.Variable {name}
      arguments <- traverse (Evidence.generate context) (toList instanciation)
      pure [statement, done (evaluate False expression arguments)]
  Selector
    { selector = Selector.Index {selectorIndex},
      argument,
      selectorInfo = EntryInfo {strict}
    } -> do
      (statements, object) <- generate context argument
      let select =
            Javascript.Member
              { object,
                field = Mangle.fields !! (selectorIndex + 1)
              }
      pure $ statements ++ [done (evaluate (Info.strict strict) select [])]
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    result <- generateInto context target letBody
    pure $ declarations ++ result
  Lambda {body} -> do
    name <- fresh context
    context <- pure $ singleBinding name context
    (statements, result) <- generate context body
    let returnx = Javascript.Return result
    pure
      [ done
          Javascript.Arrow
            { parameters = [name],
              body = statements ++ [returnx]
            }
      ]
  Call {function, argument} -> do
    (statements, function) <- generate context function
    argument <- thunk context Done argument
    let call =
          Javascript.Call
            { function,
              arguments = [argument]
            }
    pure $ statements ++ [done call]
  Join {statements} -> Statements.generate context target statements
  -- todo use big ints
  Integer {integer} ->
    pure
      [ done
          Javascript.BigInt
            { bigInt = integer
            }
      ]
  Hook {hook} -> Hook.generateInto context target hook
  Newtype {argument} -> generateInto context target argument
  where
    done value =
      Javascript.Expression
        ( Javascript.Assign
            { target,
              value
            }
        )

-- |
-- Is the thunk a part of a binding group or not?
data Binder
  = Done
  | Group

thunk :: Context s scope -> Binder -> Expression scope -> ST s Javascript.Expression
thunk context Done Variable {variable, instanciation = Instanciation instanciation}
  | null instanciation,
    Term.Binding {name, strict = False} <- context !- variable = do
      name <- symbol context name
      pure Javascript.Variable {name}
thunk context _ value = do
  body <- generateInto context done value
  pure (delay body)

declaration :: Context s scope -> SchemeOver Expression scope -> ST s Javascript.Expression
declaration context scheme@SchemeOver {result = expression} = do
  let constraintCount = SchemeOver.constraintCount scheme
  fresh <- Vector.replicateM constraintCount (Context.fresh context)
  context <- pure $ Context.evidenceBindings fresh context
  if
    | 0 <- constraintCount -> thunk context Group expression
    | otherwise -> do
        (statements, result) <- generate context expression
        pure $
          Javascript.Arrow
            { parameters = toList fresh,
              body = statements ++ [Javascript.Return result]
            }
