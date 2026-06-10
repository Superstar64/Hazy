module Core.Tree.Expression where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Temporary.Definition (Definition (Definition))
import {-# SOURCE #-} qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (Bound))
import qualified Core.Temporary.Function as Function
import qualified Core.Temporary.Pattern as Pattern
import qualified Core.Temporary.RightHandSide as RightHandSide
import {-# SOURCE #-} qualified Core.Tree.Builtin.Applicative as Builtin (applicative)
import {-# SOURCE #-} qualified Core.Tree.Builtin.Eq as Builtin (eq)
import {-# SOURCE #-} qualified Core.Tree.Builtin.Fractional as Builtin (fractional)
import {-# SOURCE #-} qualified Core.Tree.Builtin.Monad as Builtin (monad)
import {-# SOURCE #-} qualified Core.Tree.Builtin.MonadFail as Builtin (monadFail)
import {-# SOURCE #-} qualified Core.Tree.Builtin.Num as Builtin (num)
import qualified Core.Tree.Class as Class
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} qualified Core.Tree.Declaration as Declaration
import {-# SOURCE #-} Core.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Core.Tree.Declarations as Declarations
import Core.Tree.EntryInfo (EntryInfo (..))
import Core.Tree.Evidence (Evidence)
import qualified Core.Tree.Evidence as Evidence
import Core.Tree.Hook (Hook)
import Core.Tree.Instanciation (Instanciation (..))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.MethodInfo (MethodInfo)
import Core.Tree.Statements (Statements)
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Type
import Data.Foldable (toList)
import qualified Data.Kind
import Data.List.Reverse (List (..))
import qualified Data.List.Reverse as Reverse
import Data.Ratio (denominator, numerator)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Strict.Vector2 as Strict.Vector2
import Data.Text (unpack)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.Simple.ConstructorInfo as Semantic (ConstructorInfo (ConstructorInfo))
import qualified Semantic.Check.Simple.ConstructorInfo as Semantic.ConstructorInfo
import Semantic.Check.Simple.SelectorInfo (Select (..), SelectorInfo (..))
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Index.Evidence
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector (Index (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.CallHead as Semantic (CallHead (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition as Semantic (Definition)
import Semantic.Tree.Expression (Explicit (Known))
import qualified Semantic.Tree.Expression as Semantic (Expression (..))
import qualified Semantic.Tree.ExpressionField as Semantic (Field (Field))
import qualified Semantic.Tree.ExpressionField as Semantic.Field
import qualified Semantic.Tree.RightHandSide as Semantic (RightHandSide)
import qualified Semantic.Tree.Statements as Semantic (Evidence, Statements, Syntax)
import qualified Semantic.Tree.Statements as Semantic.Statements
import Prelude hiding (fail)

data Expression scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Instanciation scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        arguments :: !(Strict.Vector (Expression scope)),
        constructorInfo :: !(ConstructorInfo scope)
      }
  | Selector
      { selector :: !(Selector.Index scope),
        argument :: !(Expression scope),
        selectorInfo :: !(EntryInfo scope)
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Evidence scope),
        instanciation :: !(Instanciation scope),
        methodInfo :: !(MethodInfo scope)
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
    Selector {selector, argument, selectorInfo} ->
      Selector
        { selector = Substitute.map category selector,
          argument = Substitute.map category argument,
          selectorInfo = Substitute.map category selectorInfo
        }
    Constructor {constructor, arguments, constructorInfo} ->
      Constructor
        { constructor = Substitute.map category constructor,
          arguments = Substitute.map category <$> arguments,
          constructorInfo = Substitute.map category constructorInfo
        }
    Method {method, evidence, instanciation, methodInfo} ->
      Method
        { method = Substitute.map category method,
          evidence = Substitute.map category evidence,
          instanciation = Substitute.map category instanciation,
          methodInfo = Substitute.map category methodInfo
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

nil :: Expression scope
nil =
  Constructor
    { constructor = Constructor.nil,
      arguments = Strict.Vector.empty,
      constructorInfo = ConstructorInfo {entries = Strict.Vector.empty}
    }

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

purex :: Evidence scope -> Expression scope -> Expression scope
purex evidence value =
  Method
    { method = Method.pure,
      evidence,
      instanciation = Instanciation Strict.Vector.empty,
      methodInfo = Class.info Builtin.applicative
    }
    `call` value

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
    `call` nil

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

float_ :: Rational -> Evidence scope -> Expression scope
float_ float evidence =
  Call
    { function =
        Method
          { method = Method.fromRational,
            evidence,
            instanciation = Instanciation Strict.Vector.empty,
            methodInfo = Class.info Builtin.fractional
          },
      argument =
        Constructor
          { constructor = Constructor.makeRatio,
            arguments =
              Strict.Vector.fromList
                [ Integer {integer = numerator float},
                  Integer {integer = denominator float}
                ],
            constructorInfo =
              ConstructorInfo
                { entries =
                    Strict.Vector.fromList
                      [ EntryInfo {strict = Type.Constructor Type2.Strict},
                        EntryInfo {strict = Type.Constructor Type2.Strict}
                      ]
                }
          }
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

ifx :: Expression scope -> Expression scope -> Expression scope -> Expression scope
ifx condition thenx =
  guard
    ( Statements.bind
        ( Pattern.Match
            { match =
                Pattern.Constructor
                  { constructor = Constructor.true,
                    patterns = Strict.Vector.empty,
                    constructorInfo = Semantic.ConstructorInfo {entries = Strict.Vector.empty}
                  },
              irrefutable = False
            }
        )
        condition
        ( Statements.Done
            { done = shift thenx
            }
        )
    )

class Simplify source where
  simplify :: source Normal Check scope -> Expression scope

instance Simplify Semantic.Expression where
  simplify expression = simplifyWith expression []

simplifyCallHead :: Semantic.CallHead Check scope -> Expression scope
simplifyCallHead = \case
  Semantic.Variable {variable, instanciation = Solved instanciation} ->
    Variable
      { variable = Term.from variable,
        instanciation
      }
  Semantic.Selector {selector, selectorInfo = Solved selectorInfo} ->
    Lambda
      { body = simplifySelector (shift selector) (shift selectorInfo) lambdaVariable
      }
  Semantic.Method
    { method,
      evidence = Solved evidence,
      instanciation = Solved instanciation,
      methodInfo = Solved methodInfo
    } ->
      Method
        { method,
          evidence,
          instanciation,
          methodInfo
        }
  Semantic.Constructor {constructor, constructorInfo = Solved constructorInfo} ->
    simplifyConstructor constructor constructorInfo 0 Reverse.Nil

instance Simplify Semantic.Definition where
  simplify = Definition.desugar . Definition.simplify

instance Simplify Semantic.RightHandSide where
  simplify rightHandSide =
    Join
      { statements =
          RightHandSide.desugar $ RightHandSide.simplify rightHandSide
      }

type Proxy :: Semantic.Syntax -> Data.Kind.Type
data Proxy syntax = Proxy

class Monadic syntax where
  action :: Semantic.Evidence syntax scope -> Expression scope -> Expression scope -> Expression scope
  monad :: Bool -> Semantic.Evidence syntax scope -> Evidence scope
  lift :: Proxy syntax -> Expression scope -> Expression scope

instance Monadic 'Semantic.Statements.Do where
  action (Semantic.Statements.Monad evidence) = run evidence
  monad _ (Semantic.Statements.Monad evidence) = evidence
  lift _ = id

instance Monadic 'Semantic.Statements.Comprehension where
  action Semantic.Statements.List check thenx = ifx check thenx nil
  monad fail Semantic.Statements.List =
    if fail
      then
        Evidence.Variable
          { variable = Index.Evidence.Builtin Index.Evidence.MonadFailList,
            instanciation = Instanciation.empty
          }
      else
        Evidence.Variable
          { variable = Index.Evidence.Builtin Index.Evidence.MonadList,
            instanciation = Instanciation.empty
          }
  lift _ =
    purex
      Evidence.Variable
        { variable = Index.Evidence.Builtin Index.Evidence.ApplicativeList,
          instanciation = Instanciation.empty
        }

instance (Monadic syntax) => Simplify (Semantic.Statements syntax) where
  simplify = \case
    Semantic.Statements.Done {done} -> lift (Proxy :: Proxy syntax) (simplify done)
    Semantic.Statements.Let {declarations, body} ->
      Let
        { declarations = Declarations.simplify declarations,
          letBody = simplify body
        }
    Semantic.Statements.Run {evidence = Solved evidence, effect, after} ->
      action evidence (simplify effect) (simplify after)
    Semantic.Statements.Bind {patternx, evidence = Solved evidence, effect, thenx, fail} ->
      bind fail monadic (simplify effect) $
        Definition.desugar $
          if fail
            then Definition.Alternative {definition, alternative}
            else Definition.Definition {definition}
      where
        monadic = monad fail evidence
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
                              { done = failx (shift monadic)
                              }
                        }
                  }
            }

simplifyConstructor ::
  Constructor.Index scope ->
  Semantic.ConstructorInfo scope ->
  Int ->
  List (Expression scope) ->
  Expression scope
simplifyConstructor constructor info argumentCount arguments
  | Semantic.ConstructorInfo.entryCount info > argumentCount =
      Lambda
        { body =
            simplifyConstructor
              (shift constructor)
              (shift info)
              (argumentCount + 1)
              (fmap shift arguments :> lambdaVariable)
        }
simplifyConstructor constructor info argumentCount arguments
  | arguments <- Strict.Vector.fromListN argumentCount (toList arguments) =
      simplifyConstructorExact constructor info arguments

simplifyConstructorExact ::
  Constructor.Index scope ->
  Semantic.ConstructorInfo scope ->
  Strict.Vector (Expression scope) ->
  Expression scope
simplifyConstructorExact constructor Semantic.ConstructorInfo {entries} arguments =
  Constructor
    { constructor,
      arguments,
      constructorInfo = ConstructorInfo {entries}
    }
simplifyConstructorExact constructor Semantic.ConstructorInfo.Newtype arguments =
  Newtype
    { constructor,
      argument = Strict.Vector.head arguments,
      direction = Construct
    }

simplifySelector ::
  Selector.Index scope ->
  SelectorInfo scope ->
  Expression scope ->
  Expression scope
simplifySelector selector Uniform {strict} argument =
  Selector
    { selector,
      argument,
      selectorInfo = EntryInfo {strict}
    }
simplifySelector (Selector.Index typeIndex _) Disjoint {select} argument =
  Join {statements = mconcat statements}
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
                        (Semantic.ConstructorInfo.entryCount constructorInfo)
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
  Semantic.Expression Normal Check scope ->
  [Expression scope] ->
  Expression scope
simplifyWith Semantic.Call {function, argument} arguments =
  simplifyWith function (simplify argument : arguments)
simplifyWith
  Semantic.CallHead
    { callHead = Semantic.Constructor {constructor, constructorInfo = Solved constructorInfo}
    }
  arguments =
    simplifyConstructor constructor constructorInfo (length arguments) (Reverse.fromList arguments)
simplifyWith expression arguments@(_ : _) =
  foldl Call (simplify expression) arguments
simplifyWith expression [] = case expression of
  Semantic.CallHead {callHead} -> simplifyCallHead callHead
  Semantic.Record {constructor, constructorInfo = Solved constructorInfo, fields} ->
    simplifyConstructorExact constructor constructorInfo arguments
    where
      entryCount = Semantic.ConstructorInfo.entryCount constructorInfo
      arguments = Strict.Vector.generate entryCount $ \index' ->
        case [ expression
             | Semantic.Field
                 { index,
                   expression
                 } <-
                 toList fields,
               index == index'
             ] of
          [] -> Join {statements = Statements.Bottom}
          fields -> simplify $ last fields
  Semantic.Integer {integer, evidence = Solved evidence} -> integer_ integer evidence
  Semantic.Float {float, evidence = Solved evidence} -> float_ float evidence
  Semantic.Tuple {elements} ->
    Constructor
      { constructor = Constructor.tuple (length elements),
        arguments = simplify <$> Strict.Vector2.toVector elements,
        constructorInfo =
          ConstructorInfo
            { entries =
                Strict.Vector.replicate
                  (length elements)
                  EntryInfo
                    { strict = Type.Constructor Type2.Lazy
                    }
            }
      }
  Semantic.List {items} ->
    foldr (cons . simplify) nil items
  Semantic.Comprehension {statements} -> simplify statements
  Semantic.Let {declarations, letBody} ->
    Let
      { declarations = Declarations.simplify declarations,
        letBody = simplify letBody
      }
  Semantic.If {condition, thenx, elsex} ->
    guard
      ( Statements.bind
          ( Pattern.Match
              { match =
                  Pattern.Constructor
                    { constructor = Constructor.true,
                      patterns = Strict.Vector.empty,
                      constructorInfo = Semantic.ConstructorInfo {entries = Strict.Vector.empty}
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
  Semantic.Case {scrutinee, cases}
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
  Semantic.LambdaCase {cases}
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
  Semantic.Lambda {parameter, body} ->
    let definition =
          Bound
            { patternx = Pattern.simplify parameter,
              body = Function.simplify body
            }
     in Definition.desugar Definition {definition}
  Semantic.MultiwayIf {branches} ->
    Join
      { statements =
          foldr1
            Statements.Branch
            (RightHandSide.desugar . RightHandSide.simplify <$> branches)
      }
  Semantic.Character {character} ->
    Character {character}
  Semantic.String {string} ->
    foldr (cons . Character) nil (unpack string)
  Semantic.Do {dox} -> simplify dox
  Semantic.Annotation {expression = Known expression, annotation, instanciation = Solved instanciation} ->
    Let
      { declarations = Declarations.single $ shift $ Declaration.annotation expression annotation,
        letBody =
          Variable
            { variable = Term.Declaration 0,
              instanciation = shift instanciation
            }
      }
  Semantic.RightSection {left, right} -> do
    Lambda
      { body =
          Call
            { function =
                Call
                  { function = shift $ simplifyCallHead left,
                    argument = lambdaVariable
                  },
              argument = shift $ simplify right
            }
      }
  Semantic.Negate {evidence = Solved evidence, negative} ->
    Call
      { function =
          Method
            { method = Method.negate,
              evidence,
              instanciation = Instanciation Strict.Vector.empty,
              methodInfo = Class.info Builtin.num
            },
        argument = simplify negative
      }
  where
    cons head tail =
      Constructor
        { constructor = Constructor.cons,
          arguments = Strict.Vector.fromList [head, tail],
          constructorInfo =
            ConstructorInfo
              { entries =
                  Strict.Vector.fromList
                    [ EntryInfo {strict = Type.Constructor Type2.Lazy},
                      EntryInfo {strict = Type.Constructor Type2.Lazy}
                    ]
              }
        }
    nil =
      Constructor
        { constructor = Constructor.nil,
          arguments = Strict.Vector.empty,
          constructorInfo = ConstructorInfo {entries = Strict.Vector.empty}
        }
