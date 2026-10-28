module Core.Tree.Expression where

import {-# SOURCE #-} qualified Builtin.Applicative as Applicative
import {-# SOURCE #-} qualified Builtin.Eq as Eq
import {-# SOURCE #-} qualified Builtin.Fractional as Fractional
import {-# SOURCE #-} qualified Builtin.Monad as Monad
import {-# SOURCE #-} qualified Builtin.MonadFail as MonadFail
import {-# SOURCE #-} qualified Builtin.Num as Num
import {-# SOURCE #-} qualified Builtin.Ord as Ord
import {-# SOURCE #-} qualified Builtin.Semigroup as Semigroup
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Temporary.Definition (Definition (Definition))
import {-# SOURCE #-} qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (Bound))
import qualified Core.Temporary.Function as Function
import qualified Core.Temporary.RightHandSide as RightHandSide
import qualified Core.Tree.Class as Class
import {-# SOURCE #-} qualified Core.Tree.Declaration as Declaration
import {-# SOURCE #-} Core.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Core.Tree.Declarations as Declarations
import Core.Tree.Evidence (Evidence)
import qualified Core.Tree.Evidence as Evidence
import Core.Tree.Instanciation (Instanciation (..))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.Statements (Statements)
import qualified Core.Tree.Statements as Statements
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import Core.Tree.TypeLambda (TypeLambda)
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Kind
import Data.List.Reverse (List (..))
import qualified Data.List.Reverse as Reverse
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.Info.Constructor as Semantic (ConstructorInfo (..))
import qualified Semantic.Check.Info.Constructor as Semantic.ConstructorInfo
import Semantic.Check.Info.Entry (EntryInfo (..))
import Semantic.Check.Info.Method (MethodInfo (..))
import Semantic.Check.Info.Selector (Select (..), SelectorInfo (..))
import Semantic.Check.Info.Update (Update (..))
import qualified Semantic.Check.Info.Update as Semantic (UpdateInfo (..))
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Index.Evidence
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector (Index (..))
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.CallHead as Semantic (CallHead (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Declarations as Semantic.Declarations
import qualified Semantic.Tree.Definition as Semantic (Definition)
import Semantic.Tree.Expression (Explicit (Known))
import qualified Semantic.Tree.Expression as Semantic (Expression (..), Field (..))
import qualified Semantic.Tree.Pattern as Pattern
import qualified Semantic.Tree.RightHandSide as Semantic (RightHandSide)
import qualified Semantic.Tree.Statements as Semantic (Evidence, Statements, Syntax)
import qualified Semantic.Tree.Statements as Semantic.Statements
import qualified Syntax.StringLiteral as StringLiteral
import Prelude hiding (compare, fail)

data Expression scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Instanciation scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        arguments :: !(Strict.Vector (TypeLambda scope))
      }
  | Selector
      { selector :: !(Selector.Index scope),
        argument :: !(Expression scope),
        instanciation :: !(Instanciation scope)
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Evidence scope),
        instanciation :: !(Instanciation scope),
        constraintCount :: !Int
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

instance Shift0.Functor Expression where
  map = Shift.mapDefault

instance Shift.Functor Expression where
  map = Substitute.mapDefault

instance Substitute.Functor Expression where
  map category = \case
    Variable {variable, instanciation} ->
      Variable
        { variable = Substitute.map category variable,
          instanciation = Substitute.map category instanciation
        }
    Selector {selector, argument, instanciation} ->
      Selector
        { selector = Substitute.map category selector,
          argument = Substitute.map category argument,
          instanciation = Substitute.map category instanciation
        }
    Constructor {constructor, arguments} ->
      Constructor
        { constructor = Substitute.map category constructor,
          arguments = Substitute.map category <$> arguments
        }
    Method {method, evidence, instanciation, constraintCount} ->
      Method
        { method = Substitute.map category method,
          evidence = Substitute.map category evidence,
          instanciation = Substitute.map category instanciation,
          constraintCount
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
    Newtype {constructor, argument, direction} ->
      Newtype
        { constructor = Substitute.map category constructor,
          argument = Substitute.map category argument,
          direction
        }

monoVariable :: Type scope -> Term.Index scope -> Expression scope
monoVariable levity variable =
  Variable
    { variable,
      instanciation = Instanciation.Mono {levity}
    }

bindingVariable :: Type scope -> Expression (Scope.SimpleDeclaration ':+ scope)
bindingVariable levity = monoVariable (shift levity) $ Term.SimpleDeclaration

lambdaVariable :: Expression (Scope.SimpleDeclaration ':+ scopes)
lambdaVariable = bindingVariable Type.lazy

patternVariable :: Type scope -> Expression (Scope.Pattern ':+ scope)
patternVariable levity = monoVariable (shift levity) $ Term.Pattern Term.At

patternVariableAt :: Type scope -> Int -> Expression (Scope.SimplePattern ':+ scope)
patternVariableAt levity index = monoVariable (shift levity) $ Term.SimplePattern index

patternVariableAt' :: Type scope -> Int -> Expression (Scope.Pattern ':+ scope)
patternVariableAt' levity index = monoVariable (shift levity) $ Term.Pattern $ Term.Select index Term.At

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
      arguments = Strict.Vector.empty
    }

eqChar :: Expression scope -> Expression scope -> Expression scope
eqChar =
  eq
    Evidence.Variable
      { variable = Index.Evidence.Direct Type2.Eq Type2.Char,
        instanciation = Instanciation.monoLazy
      }

eq :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
eq evidence left right =
  Method
    { method = Method.equal,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Eq.definition
    }
    `call` left
    `call` right

lessThenEqual :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
lessThenEqual evidence left right =
  Method
    { method = Method.lessThenEqual,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Ord.definition
    }
    `call` left
    `call` right

lessThenEqualInt :: Expression scope -> Expression scope -> Expression scope
lessThenEqualInt =
  lessThenEqual
    Evidence.Variable
      { variable = Index.Evidence.Direct Type2.Ord Type2.Int,
        instanciation = Instanciation.monoLazy
      }

compare :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
compare evidence left right =
  Method
    { method = Method.compare,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Ord.definition
    }
    `call` left
    `call` right

compareInt :: Expression scope -> Expression scope -> Expression scope
compareInt =
  compare
    Evidence.Variable
      { variable = Index.Evidence.Direct Type2.Ord Type2.Int,
        instanciation = Instanciation.monoLazy
      }

orderingEqual :: Expression scope
orderingEqual =
  Constructor
    { constructor = Constructor.eq,
      arguments = Strict.Vector.empty
    }

orderingCombine :: Expression scope -> Expression scope -> Expression scope
orderingCombine left right =
  Method
    { method = Method.combine,
      evidence =
        Evidence.Variable
          { variable = Index.Evidence.Direct Type2.Semigroup Type2.Ordering,
            instanciation = Instanciation.monoLazy
          },
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Semigroup.definition
    }
    `call` left
    `call` right

minBound, maxBound :: Evidence scope -> Expression scope
minBound evidence =
  Method
    { method = Method.minBound,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Eq.definition
    }
maxBound evidence =
  Method
    { method = Method.maxBound,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Eq.definition
    }

purex :: Evidence scope -> Expression scope -> Expression scope
purex evidence value =
  Method
    { method = Method.pure,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Applicative.definition
    }
    `call` value

run :: Evidence scope -> Expression scope -> Expression scope -> Expression scope
run evidence ignore thenx =
  Method
    { method = Method.thenx,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Monad.definition
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
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info Monad.definition
    }
    `call` input
    `call` output

failx :: Evidence scope -> Expression scope
failx evidence =
  Method
    { method = Method.fail,
      evidence,
      instanciation = Instanciation.monoLazy,
      constraintCount = Class.info MonadFail.definition
    }
    `call` nil

int :: Int -> Expression scope
int int =
  integer_
    (toInteger int)
    Evidence.Variable
      { variable = Index.Evidence.Direct Type2.Num Type2.Int,
        instanciation = Instanciation.monoLazy
      }

integer_ :: Integer -> Evidence scope -> Expression scope
integer_ integer evidence =
  Call
    { function =
        Method
          { method = Method.fromInteger,
            evidence,
            instanciation = Instanciation.monoLazy,
            constraintCount = Class.info Num.definition
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
            instanciation = Instanciation.monoLazy,
            constraintCount = Class.info Fractional.definition
          },
      argument =
        Constructor
          { constructor = Constructor.makeRatio,
            arguments =
              Strict.Vector.fromList
                [ TypeLambda.mono Type.strict Integer {integer = numerator float},
                  TypeLambda.mono Type.strict Integer {integer = denominator float}
                ]
          }
    }

bool :: Bool -> Expression scope
bool value =
  Constructor
    { constructor = if value then Constructor.true else Constructor.false,
      arguments = Strict.Vector.empty
    }

true :: Expression scope
true = bool True

false :: Expression scope
false = bool False

infixr 3 &&

(&&) :: Expression scope -> Expression scope -> Expression scope
left && right = ifx left right false

infixr 2 ||

(||) :: Expression scope -> Expression scope -> Expression scope
left || right = ifx left true right

guard :: Statements scope -> Expression scope -> Expression scope
guard left done =
  Join
    Statements.Branch
      { left,
        right = Statements.Done {done}
      }

ifx :: Expression scope -> Expression scope -> Expression scope -> Expression scope
ifx condition thenx = guard (Statements.ifx condition thenx)

infixl 9 `call`

call :: Expression scope -> Expression scope -> Expression scope
call function argument = Call {function, argument}

class Simplify source where
  simplify :: source Normal Check scope -> Expression scope
  simplifyWith :: source Normal Check scope -> Strict.Vector (Expression scope) -> Expression scope
  simplifyWith main arguments = foldl Call (simplify main) arguments

instance Simplify Semantic.CallHead where
  simplify = \case
    Semantic.Variable {variable, instanciation = Solved instanciation} ->
      Variable
        { variable,
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
        methodInfo = Solved MethodInfo {constraintCount}
      } ->
        Method
          { method,
            evidence,
            instanciation,
            constraintCount
          }
    Semantic.Constructor {constructor, constructorInfo = Solved constructorInfo} ->
      simplifyConstructor constructor constructorInfo 0 Reverse.Nil
  simplifyWith callHead arguments = case callHead of
    Semantic.Constructor {constructor, constructorInfo = Solved constructorInfo}
      | Semantic.ConstructorInfo.entryCount constructorInfo == length arguments ->
          simplifyConstructorExact constructor constructorInfo arguments
      | otherwise ->
          simplifyConstructor
            constructor
            constructorInfo
            (length arguments)
            (Reverse.fromList $ toList arguments)
    Semantic.Selector {selector, selectorInfo = Solved selectorInfo}
      | (argument : arguments) <- toList arguments ->
          foldl Call (simplifySelector selector selectorInfo argument) arguments
    _ -> foldl Call (simplify callHead) arguments

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
          { variable = Index.Evidence.Direct Type2.MonadFail Type2.List,
            instanciation = Instanciation.monoLazy
          }
      else
        Evidence.Variable
          { variable = Index.Evidence.Direct Type2.Monad Type2.List,
            instanciation = Instanciation.monoLazy
          }
  lift _ =
    purex
      Evidence.Variable
        { variable = Index.Evidence.Direct Type2.Applicative Type2.List,
          instanciation = Instanciation.monoLazy
        }

instance (Monadic syntax) => Simplify (Semantic.Statements syntax) where
  simplify = \case
    Semantic.Statements.Done {done} -> lift (Proxy :: Proxy syntax) (simplify done)
    Semantic.Statements.Let {declarations = Semantic.Declarations.Local declarations, body} ->
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
            { patternx = Pattern.anonymize patternx,
              body = Function.Plain {plain = RightHandSide.Done {done = simplify thenx}}
            }
        alternative =
          Definition
            { definition =
                Function.Bound
                  { patternx =
                      Pattern.Wildcard
                        { startPosition = (),
                          levity = Solved Type.lazy,
                          names = Map.empty
                        },
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
      arguments = Strict.Vector.zipWith argument entries arguments
    }
  where
    argument EntryInfo {levity} field = TypeLambda.mono levity field
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
simplifySelector selector Uniform {levity} argument =
  Selector
    { selector,
      argument,
      instanciation = Instanciation.Mono {levity}
    }
simplifySelector (Selector.Index typeIndex _) Disjoint {select} argument =
  Join {statements = mconcat statements}
  where
    generate index Select {selectIndex, constructorInfo} =
      case constructorInfo of
        Semantic.ConstructorInfo {entries} ->
          Statements.Bind
            { constructor = Constructor.Index typeIndex index,
              entryCount = length entries,
              check = argument,
              thenx = case selectIndex of
                Strict.Just index
                  | EntryInfo {levity} <- entries Strict.Vector.! index ->
                      Statements.Done
                        { done = monoVariable (shift levity) $ Term.SimplePattern index
                        }
                Strict.Nothing -> Statements.Bottom
            }
        Semantic.Newtype ->
          Statements.Done
            { done =
                Newtype
                  { constructor = Constructor.Index typeIndex index,
                    argument,
                    direction = Destruct
                  }
            }

    statements = zipWith generate [0 ..] $ toList select

instance Simplify Semantic.Expression where
  simplify = \case
    Semantic.CallHead {callHead, arguments} ->
      simplifyWith callHead (fmap simplify arguments)
    Semantic.Call {function, argument} ->
      Call
        { function = simplify function,
          argument = simplify argument
        }
    Semantic.Record {constructor, constructorInfo = Solved constructorInfo, fields} ->
      simplifyConstructorExact constructor constructorInfo arguments
      where
        entryCount = Semantic.ConstructorInfo.entryCount constructorInfo
        arguments = Strict.Vector.generate entryCount $ \index' ->
          case [ field
               | Semantic.Field
                   { index,
                     field
                   } <-
                   toList fields,
                 index == index'
               ] of
            [] -> Join {statements = Statements.Bottom}
            fields -> simplify $ last fields
    Semantic.Update
      { base,
        updateType,
        updates,
        updateInfo = Solved Semantic.UpdateInfo {updateInfo},
        permissive
      } ->
        Call
          { function =
              Lambda
                { body =
                    Join
                      { statements = foldr1 (<>) $ Strict.Vector.imap update updateInfo
                      }
                },
            argument = simplify base
          }
        where
          update constructorIndex Update {constructorInfo, selectorIndexes} =
            case constructorInfo of
              Semantic.ConstructorInfo {entries} ->
                Statements.Bind
                  { constructor,
                    entryCount = length entries,
                    check = lambdaVariable,
                    thenx =
                      Statements.Done
                        { done =
                            if
                              | not permissive, any illegal updates -> Join {statements = Statements.Bottom}
                              | otherwise ->
                                  simplifyConstructorExact
                                    (shift constructor)
                                    (shift $ shift constructorInfo)
                                    (Strict.Vector.imap generate selectorIndexes)
                        }
                  }
                where
                  illegal Semantic.Field {index} = Strict.Just index `notElem` selectorIndexes
                  generate index selectorIndex
                    | EntryInfo {levity} <- entries Strict.Vector.! index =
                        case [ shift $ shift $ simplify field
                             | Semantic.Field {index, field} <- toList updates,
                               selectorIndex == Strict.Just index
                             ] of
                          [] -> monoVariable (shift $ shift levity) $ Term.SimplePattern index
                          expressions -> last expressions
              Semantic.Newtype ->
                Statements.Done
                  { done =
                      Newtype
                        { constructor,
                          argument = last [shift $ simplify field | Semantic.Field {field} <- toList updates],
                          direction = Construct
                        }
                  }
            where
              constructor =
                Constructor.Index
                  { typeIndex = shift updateType,
                    constructorIndex
                  }
    Semantic.Integer {integer, evidence = Solved evidence} -> integer_ integer evidence
    Semantic.Float {float, evidence = Solved evidence} -> float_ float evidence
    Semantic.Tuple {elements} ->
      Constructor
        { constructor = Constructor.tuple (length elements),
          arguments = Strict.Vector.map argument elements
        }
      where
        argument element = TypeLambda.mono Type.lazy (simplify element)
    Semantic.List {items} ->
      foldr (cons . simplify) nil items
    Semantic.Comprehension {statements} -> simplify statements
    Semantic.Let {declarations = Semantic.Declarations.Local declarations, letBody} ->
      Let
        { declarations = Declarations.simplify declarations,
          letBody = simplify letBody
        }
    Semantic.If {condition, thenx, elsex} ->
      ifx (simplify condition) (simplify thenx) (simplify elsex)
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
              { patternx = Pattern.anonymize parameter,
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
      foldr (cons . Character) nil (StringLiteral.unpack string)
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
                    { function = shift $ simplify left,
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
                instanciation = Instanciation.monoLazy,
                constraintCount = Class.info Num.definition
              },
          argument = simplify negative
        }
    where
      cons head tail =
        Constructor
          { constructor = Constructor.cons,
            arguments =
              Strict.Vector.fromList
                [ TypeLambda.mono Type.lazy head,
                  TypeLambda.mono Type.lazy tail
                ]
          }
      nil =
        Constructor
          { constructor = Constructor.nil,
            arguments = Strict.Vector.empty
          }
