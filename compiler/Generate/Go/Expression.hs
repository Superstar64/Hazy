module Generate.Go.Expression where

import Control.Monad (zipWithM)
import Control.Monad.ST (ST)
import Core.Tree.Constraints (ConstraintCount (..))
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import Core.Tree.EntryInfo (EntryInfo (EntryInfo))
import qualified Core.Tree.EntryInfo
import Core.Tree.Expression (Expression (..))
import Core.Tree.Instanciation (Instanciation (Instanciation))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.MethodInfo (MethodInfo (..))
import Core.Tree.SchemeOver (SchemeOver (..))
import qualified Core.Tree.SchemeOver as SchemeOver
import Data.Char (ord)
import Data.Foldable (toList)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Generate.Binding.Term as Term
import Generate.Context (Context (..), fresh, singleBinding, symbol, (!-))
import qualified Generate.Context as Context
import {-# SOURCE #-} qualified Generate.Go.Declarations as Declarations
import qualified Generate.Go.Evidence as Evidence
import qualified Generate.Go.Hook as Hook
import qualified Generate.Go.Statements as Statements
import qualified Generate.Info as Info
import qualified Generate.Mangle as Mangle
import Generate.Target (Target (..), finish)
import qualified Javascript.Tree.Expression as Javascript (Expression (..))
import qualified Javascript.Tree.Field as Javascript (Field (..))
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import Semantic.Scope (SimpleDeclaration)
import qualified Semantic.Scope as Scope

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

strict :: Javascript.Expression -> Javascript.Expression
strict literal =
  let fields =
        [ ( Mangle.lazy,
            Javascript.Literal
              { literal = Javascript.Number {number = 0}
              }
          ),
          ( Mangle.value,
            Javascript.Literal
              { literal
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

implicit :: Context s scope -> Bool -> Javascript.Expression -> Instanciation scope -> ST s Javascript.Expression
implicit _ False thunk Instanciation.Mono = pure $ force thunk
implicit _ True value Instanciation.Mono = pure $ value
implicit context _ function (Instanciation arguments) = do
  arguments <- traverse (Evidence.generate context) (toList arguments)
  pure Javascript.Call {function, arguments}

generateConstructor ::
  Context s scope ->
  Constructor.Index scope ->
  Strict.Vector (Expression scope) ->
  ConstructorInfo scope ->
  ST s ([Javascript.Statement 'True], Javascript.Expression)
generateConstructor context Constructor.Index {constructorIndex} arguments ConstructorInfo {entries} = do
  let process EntryInfo {strict} argument =
        if Info.strict strict
          then do
            (extra, value) <- generatePure context argument
            pure (extra, value)
          else do
            value <- thunk context Done argument
            pure ([], value)
  (extra, arguments) <- unzip <$> zipWithM process (toList entries) (toList arguments)
  let tag = Javascript.Number constructorIndex
      elements = map Javascript.Literal (tag : arguments)
      fields = zip Mangle.fields elements
      object = Javascript.Object {fields}
  pure (concat extra, object)

generateLambda :: Context s scope -> Expression (SimpleDeclaration 'Scope.:+ scope) -> ST s Javascript.Expression
generateLambda context body = do
  name <- fresh context
  context <- pure $ singleBinding name context
  statements <- generateInto context Return body
  pure
    Javascript.Arrow
      { parameters = [name],
        body = statements
      }

generateCharacter :: Char -> Javascript.Expression
generateCharacter character = Javascript.Number {number = ord character}

generateInteger :: Integer -> Javascript.Expression
generateInteger integer = Javascript.BigInt {bigInt = integer}

-- |
-- Is the thunk a part of a binding group or not?
data Binder
  = Done
  | Group

thunk :: Context s scope -> Binder -> Expression scope -> ST s Javascript.Expression
thunk context Done Variable {variable, instanciation = Instanciation.Mono}
  | Term.Binding {name, strict = False} <- context !- variable = do
      name <- symbol context name
      pure Javascript.Variable {name}
thunk context binder expression = case expression of
  Constructor {constructor, arguments, constructorInfo} -> do
    (extra, value) <- generateConstructor context constructor arguments constructorInfo
    if not (null extra)
      then do
        let body = extra ++ [Javascript.Expression Javascript.Assign {target = done, value}]
        pure (delay body)
      else do
        pure (strict value)
  Character {character} -> pure (strict $ generateCharacter character)
  Integer {integer} -> pure (strict $ generateInteger integer)
  Lambda {body} -> do
    value <- generateLambda context body
    pure $ strict value
  Newtype {argument} -> thunk context binder argument
  _ -> do
    body <- generateInto context (Assign done) expression
    pure (delay body)

generatePure,
  generateImpure ::
    Context s scope ->
    Expression scope ->
    ST s ([Javascript.Statement 'True], Javascript.Expression)
generateImpure context expression = case expression of
  Variable {variable, instanciation} -> do
    let Term.Binding {name, strict} = context !- variable
    name <- symbol context name
    let expression = Javascript.Variable {name}
    result <- implicit context strict expression instanciation
    pure ([], result)
  Method
    { method = Method.Index {methodIndex},
      evidence,
      instanciation,
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
      result <- implicit context False expression instanciation
      pure ([statement], result)
  Selector
    { selector = Selector.Index {selectorIndex},
      argument,
      selectorInfo = EntryInfo {strict}
    } -> do
      (statements, object) <- generateImpure context argument
      let select =
            Javascript.Member
              { object,
                field = Mangle.fields !! (selectorIndex + 1)
              }
      result <- implicit context (Info.strict strict) select Instanciation.Mono
      pure (statements, result)
  Call {function, argument} -> do
    (statements, function) <- generateImpure context function
    argument <- thunk context Done argument
    let call =
          Javascript.Call
            { function,
              arguments = [argument]
            }
    pure (statements, call)
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    (extra, result) <- generateImpure context letBody
    pure (declarations ++ extra, result)
  Hook {hook} -> do
    value <- Hook.generate context hook
    pure ([], value)
  _ -> generatePure context expression
generatePure context expression = case expression of
  Constructor {constructor, arguments, constructorInfo} -> do
    generateConstructor context constructor arguments constructorInfo
  Character {character} -> pure ([], generateCharacter character)
  Integer {integer} -> pure ([], generateInteger integer)
  Lambda {body} -> do
    value <- generateLambda context body
    pure ([], value)
  Newtype {argument} -> generatePure context argument
  _ -> do
    name <- fresh context
    let letx = Javascript.Let name
    body <- generateInto context (Assign Javascript.Variable {name}) expression
    pure (letx : body, Javascript.Variable {name})

generateInto ::
  Context s scope ->
  Target return ->
  Expression scope ->
  ST s [Javascript.Statement 'True]
generateInto context target = \case
  Let {declarations, letBody} -> do
    (context, declarations) <- Declarations.generate context declarations
    result <- generateInto context target letBody
    pure $ declarations ++ result
  Join {statements} -> Statements.generate context target statements
  Newtype {argument} -> generateInto context target argument
  expression -> do
    (extra, value) <- generateImpure context expression
    pure $ extra ++ [finish target value]

declaration :: Context s scope -> SchemeOver Expression scope -> ST s Javascript.Expression
declaration context scheme@SchemeOver {result = expression} = do
  let constraintCount = SchemeOver.constraintCount scheme
      actualCount = case constraintCount of
        ConstraintCount count -> count
        Null -> 0
  fresh <- Vector.replicateM actualCount (Context.fresh context)
  context <- pure $ Context.evidenceBindings fresh context
  case constraintCount of
    Null -> thunk context Group expression
    ConstraintCount {} -> do
      statements <- generateInto context Return expression
      pure
        Javascript.Arrow
          { parameters = toList fresh,
            body = statements
          }
