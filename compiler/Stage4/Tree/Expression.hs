module Stage4.Tree.Expression where

import Data.Foldable (toList)
import Data.List.Reverse (List (..))
import qualified Data.List.Reverse as Reverse
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Strict.Vector2 as Strict.Vector2
import Data.Text (unpack)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector (Index (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence as Index.Evidence
import Stage3.Tree.ConstructorInfo (ConstructorInfo (ConstructorInfo))
import qualified Stage3.Tree.ConstructorInfo as ConstructorInfo
import qualified Stage3.Tree.Definition as Stage3 (Definition)
import qualified Stage3.Tree.Do as Stage3 (Do)
import qualified Stage3.Tree.Do as Stage3.Do
import qualified Stage3.Tree.Expression as Stage3 (Expression (..))
import qualified Stage3.Tree.ExpressionField as Stage3 (Field (Field))
import qualified Stage3.Tree.ExpressionField as Stage3.Field
import qualified Stage3.Tree.RightHandSide as Stage3 (RightHandSide)
import Stage3.Tree.SelectorInfo (Select (..), SelectorInfo (..))
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import {-# SOURCE #-} Stage4.Temporary.Definition (Definition (Definition))
import {-# SOURCE #-} qualified Stage4.Temporary.Definition as Definition
import Stage4.Temporary.Function (Function (Bound))
import qualified Stage4.Temporary.Function as Function
import qualified Stage4.Temporary.Pattern as Pattern
import qualified Stage4.Temporary.RightHandSide as RightHandSide
import {-# SOURCE #-} qualified Stage4.Tree.Builtin.Eq as Builtin (eq)
import {-# SOURCE #-} qualified Stage4.Tree.Builtin.Monad as Builtin (monad)
import {-# SOURCE #-} qualified Stage4.Tree.Builtin.MonadFail as Builtin (monadFail)
import {-# SOURCE #-} qualified Stage4.Tree.Builtin.Num as Builtin (num)
import qualified Stage4.Tree.Class as Class
import {-# SOURCE #-} Stage4.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Tree.Declarations as Declarations
import Stage4.Tree.Evidence (Evidence)
import qualified Stage4.Tree.Evidence as Evidence
import Stage4.Tree.Hook (Hook)
import Stage4.Tree.Instanciation (Instanciation (..))
import qualified Stage4.Tree.Instanciation as Instanciation
import Stage4.Tree.MethodInfo (MethodInfo)
import Stage4.Tree.Statements (Statements)
import qualified Stage4.Tree.Statements as Statements
import Prelude hiding (fail)

data Expression scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Instanciation scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        arguments :: !(Strict.Vector (Expression scope))
      }
  | Selector
      { selector :: !(Selector.Index scope),
        argument :: !(Expression scope)
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Evidence scope),
        instanciation :: !(Instanciation scope),
        methodInfo :: !MethodInfo
      }
  | Integer
      { integer :: !Integer
      }
  | Character
      { character :: !Char
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Expression (Scope.Declaration ':+ scope))
      }
  | Lambda
      { body :: !(Expression (Scope.SimpleDeclaration ':+ scope))
      }
  | Call
      { function :: !(Expression scope),
        argument :: !(Expression scope)
      }
  | Join
      { statements :: !(Statements scope)
      }
  | Hook
      { hook :: !(Hook scope)
      }
  | Newtype
      { constructor :: !(Constructor.Index scope),
        argument :: !(Expression scope),
        direction :: !Direction
      }
  deriving (Show)

data Direction = Construct | Destruct
  deriving (Show)

instance Scope.Show Expression where
  showsPrec = showsPrec

instance Shift Expression where
  shift = shiftDefault

instance Shift.Functor Expression where
  map = Shift2.mapDefault

instance Shift2.Functor Expression where
  map = Substitute.mapDefault

instance Substitute.Functor Expression where
  map category = \case
    Variable {variable, instanciation} ->
      Variable
        { variable = Substitute.map category variable,
          instanciation = Substitute.map category instanciation
        }
    Selector {selector, argument} ->
      Selector
        { selector = Substitute.map category selector,
          argument = Substitute.map category argument
        }
    Constructor {constructor, arguments} ->
      Constructor
        { constructor = Substitute.map category constructor,
          arguments = Substitute.map category <$> arguments
        }
    Method {method, evidence, instanciation, methodInfo} ->
      Method
        { method = Substitute.map category method,
          evidence = Substitute.map category evidence,
          instanciation = Substitute.map category instanciation,
          methodInfo
        }
    Integer {integer} ->
      Integer
        { integer
        }
    Character {character} ->
      Character
        { character
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Substitute.map (Substitute.Over category) declarations,
          letBody = Substitute.map (Substitute.Over category) letBody
        }
    Lambda {body} ->
      Lambda
        { body = Substitute.map (Substitute.Over category) body
        }
    Call {function, argument} ->
      Call
        { function = Substitute.map category function,
          argument = Substitute.map category argument
        }
    Join {statements} ->
      Join
        { statements = Substitute.map category statements
        }
    Hook {hook} ->
      Hook
        { hook = Substitute.map category hook
        }
    Newtype {constructor, argument, direction} ->
      Newtype
        { constructor = Substitute.map category constructor,
          argument = Substitute.map category argument,
          direction
        }

monoVariable :: Term.Index scope -> Expression scope
monoVariable variable =
  Variable
    { variable,
      instanciation = Instanciation Strict.Vector.empty
    }

lambdaVariable :: Expression (Scope.SimpleDeclaration ':+ scopes)
lambdaVariable = monoVariable $ Term.SimpleDeclaration

patternVariable :: Expression (Scope.Pattern ':+ scope)
patternVariable = monoVariable $ Term.Pattern Term.At

patternVariableAt :: Int -> Expression (Scope.SimplePattern ':+ scope)
patternVariableAt index = monoVariable $ Term.SimplePattern index

character_ :: Char -> Expression scope
character_ character = Character {character}

join_ :: Statements scope -> Expression scope
join_ statements = Join {statements}

newtype_ :: Constructor.Index scope -> Expression scope -> Direction -> Expression scope
newtype_ constructor argument direction = Newtype {constructor, argument, direction}

eqChar :: Expression scope -> Expression scope -> Expression scope
eqChar =
  eq
    Evidence.Variable
      { variable = Index.Evidence.Builtin Index.Evidence.EqChar,
        instanciation = Instanciation.empty
      }

eq :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
eq evidence left right =
  Method
    { method = Method.equal,
      evidence,
      instanciation = Instanciation Strict.Vector.empty,
      methodInfo = Class.info Builtin.eq
    }
    `call` left
    `call` right

run :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
run evidence ignore thenx =
  Method
    { method = Method.thenx,
      evidence,
      instanciation = Instanciation Strict.Vector.empty,
      methodInfo = Class.info Builtin.monad
    }
    `call` ignore
    `call` thenx

bind :: Bool -> Evidence scope -> Expression scope -> Expression scope -> Expression scope
bind fail evidence input output =
  Method
    { method = Method.bind,
      evidence =
        if fail
          then Evidence.Super {base = evidence, index = 0}
          else evidence,
      instanciation = Instanciation Strict.Vector.empty,
      methodInfo = Class.info Builtin.monad
    }
    `call` input
    `call` output

failx :: Evidence scope -> Expression scope
failx evidence =
  Method
    { method = Method.fail,
      evidence,
      instanciation = Instanciation Strict.Vector.empty,
      methodInfo = Class.info Builtin.monadFail
    }
    `call` Constructor
      { constructor = Constructor.nil,
        arguments = Strict.Vector.empty
      }

integer_ :: Integer -> Evidence scope -> Expression scope
integer_ integer evidence =
  Call
    { function =
        Method
          { method = Method.fromInteger,
            evidence,
            instanciation = Instanciation Strict.Vector.empty,
            methodInfo = Class.info Builtin.num
          },
      argument = Integer {integer}
    }

infixl 9 `call`

call :: Expression scope -> Expression scope -> Expression scope
call function argument = Call {function, argument}

guard :: Statements scope -> Expression scope -> Expression scope
guard left done =
  Join
    Statements.Branch
      { left,
        right = Statements.Done {done}
      }

class Simplify source where
  simplify :: source scope -> Expression scope

instance Simplify Stage3.Expression where
  simplify expression = simplifyWith expression []

instance Simplify Stage3.Definition where
  simplify = Definition.desugar . Definition.simplify

instance Simplify Stage3.RightHandSide where
  simplify rightHandSide =
    Join
      { statements =
          RightHandSide.desugar $ RightHandSide.simplify rightHandSide
      }

instance Simplify Stage3.Do where
  simplify = \case
    Stage3.Do.Done {done} -> simplify done
    Stage3.Do.Let {declarations, letBody} ->
      Let
        { declarations = Declarations.simplify Term.Declaration declarations,
          letBody = simplify letBody
        }
    Stage3.Do.Run {evidence, effect, after} ->
      run evidence (simplify effect) (simplify after)
    Stage3.Do.Bind {patternx, evidence, effect, thenx, fail} ->
      bind fail evidence (simplify effect) $
        Definition.desugar $
          if fail
            then Definition.Alternative {definition, alternative}
            else Definition.Definition {definition}
      where
        definition =
          Bound
            { patternx = Pattern.simplify patternx,
              body = Function.Plain {plain = RightHandSide.Done {done = simplify thenx}}
            }
        alternative =
          Definition
            { definition =
                Function.Bound
                  { patternx = Pattern.Wildcard,
                    body =
                      Function.Plain
                        { plain =
                            RightHandSide.Done
                              { done = failx (shift evidence)
                              }
                        }
                  }
            }

simplifyConstructor ::
  Constructor.Index scope ->
  ConstructorInfo ->
  Int ->
  List (Expression scope) ->
  Expression scope
simplifyConstructor constructor info argumentCount arguments
  | ConstructorInfo.parameterCount info > argumentCount =
      Lambda
        { body =
            simplifyConstructor
              (shift constructor)
              info
              (argumentCount + 1)
              (fmap shift arguments :> lambdaVariable)
        }
simplifyConstructor constructor info argumentCount arguments
  | arguments <- Strict.Vector.fromListN argumentCount (toList arguments) =
      simplifyConstructorExact constructor info arguments

simplifyConstructorExact ::
  Constructor.Index scope ->
  ConstructorInfo ->
  Strict.Vector (Expression scope) ->
  Expression scope
simplifyConstructorExact constructor ConstructorInfo {} arguments =
  Constructor
    { constructor,
      arguments
    }
simplifyConstructorExact constructor ConstructorInfo.Newtype arguments =
  Newtype
    { constructor,
      argument = Strict.Vector.head arguments,
      direction = Construct
    }

simplifySelector ::
  Selector.Index scope ->
  SelectorInfo ->
  Expression scope ->
  Expression scope
simplifySelector selector Uniform {} argument = Selector {selector, argument}
simplifySelector (Selector.Index typeIndex _) Disjoint {select} argument = Join {statements = mconcat statements}
  where
    generate index Select {selectIndex, constructorInfo} =
      Statements.bind match argument get
      where
        match =
          Pattern.Match
            { irrefutable = False,
              match =
                Pattern.Constructor
                  { constructor = Constructor.Index typeIndex index,
                    patterns =
                      Strict.Vector.replicate
                        (ConstructorInfo.parameterCount constructorInfo)
                        Pattern.Wildcard,
                    constructorInfo
                  }
            }
        get =
          case selectIndex of
            Strict.Just index ->
              Statements.Done
                { done = monoVariable $ Term.Pattern (Term.Select index Term.At)
                }
            Strict.Nothing -> Statements.Bottom
    statements = zipWith generate [0 ..] $ toList select

simplifyWith ::
  Stage3.Expression scope ->
  [Expression scope] ->
  Expression scope
simplifyWith Stage3.Call {function, argument} arguments =
  simplifyWith function (simplify argument : arguments)
simplifyWith
  Stage3.Constructor {constructor, constructorInfo}
  arguments =
    simplifyConstructor constructor constructorInfo (length arguments) (Reverse.fromList arguments)
simplifyWith expression arguments@(_ : _) =
  foldl Call (simplify expression) arguments
simplifyWith expression [] = case expression of
  Stage3.Variable {variable, instanciation} ->
    Variable
      { variable = Term.from variable,
        instanciation
      }
  Stage3.Selector {selector, selectorInfo} ->
    Lambda
      { body = simplifySelector (shift selector) selectorInfo lambdaVariable
      }
  Stage3.Method {method, evidence, instanciation, methodInfo} ->
    Method
      { method,
        evidence,
        instanciation,
        methodInfo
      }
  Stage3.Record {constructor, constructorInfo, fields} ->
    simplifyConstructorExact constructor constructorInfo arguments
    where
      parameterCount = ConstructorInfo.parameterCount constructorInfo
      arguments = Strict.Vector.generate parameterCount $ \index' ->
        case [ expression
             | Stage3.Field
                 { index,
                   expression
                 } <-
                 toList fields,
               index == index'
             ] of
          [] -> Join {statements = Statements.Bottom}
          fields -> simplify $ last fields
  Stage3.Integer {integer, evidence} -> integer_ integer evidence
  Stage3.Tuple {elements} ->
    Constructor
      { constructor = Constructor.tuple (length elements),
        arguments = simplify <$> Strict.Vector2.toVector elements
      }
  Stage3.List {items} ->
    foldr (cons . simplify) nil items
  Stage3.Let {declarations, letBody} ->
    Let
      { declarations = Declarations.simplify Term.Declaration declarations,
        letBody = simplify letBody
      }
  Stage3.If {condition, thenx, elsex} ->
    guard
      ( Statements.bind
          ( Pattern.Match
              { match =
                  Pattern.Constructor
                    { constructor = Constructor.true,
                      patterns = Strict.Vector.empty,
                      constructorInfo = ConstructorInfo {parameterCount_ = 0}
                    },
                irrefutable = False
              }
          )
          (simplify condition)
          ( Statements.Done
              { done = shift $ simplify thenx
              }
          )
      )
      (simplify elsex)
  Stage3.Case {scrutinee, cases}
    | null cases ->
        Join
          { statements = Statements.Bottom
          }
    | otherwise ->
        Call
          { function =
              Definition.desugar $
                foldr1 (<>) (Definition . Function.simplify <$> toList cases),
            argument = simplify scrutinee
          }
  Stage3.LambdaCase {cases}
    | null cases ->
        Lambda
          { body =
              Join
                { statements = Statements.Bottom
                }
          }
    | otherwise ->
        Definition.desugar $
          foldr1 (<>) (Definition . Function.simplify <$> toList cases)
  Stage3.Lambda {parameter, body} ->
    let definition =
          Bound
            { patternx = Pattern.simplify parameter,
              body = Function.simplify body
            }
     in Definition.desugar Definition {definition}
  Stage3.MultiwayIf {branches} ->
    Join
      { statements =
          foldr1
            Statements.Branch
            (RightHandSide.desugar . RightHandSide.simplify <$> branches)
      }
  Stage3.Character {character} ->
    Character {character}
  Stage3.String {string} ->
    foldr (cons . Character) nil (unpack string)
  Stage3.Do {statements} -> simplify statements
  where
    cons head tail =
      Constructor
        { constructor = Constructor.cons,
          arguments = Strict.Vector.fromList [head, tail]
        }
    nil =
      Constructor
        { constructor = Constructor.nil,
          arguments = Strict.Vector.empty
        }
