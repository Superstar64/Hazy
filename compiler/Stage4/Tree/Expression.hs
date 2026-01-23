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
import qualified Stage2.Tree.Selector as Selector (Uniform (..))
import qualified Stage3.Tree.Definition as Stage3 (Definition)
import qualified Stage3.Tree.Expression as Stage3 (Expression (..))
import qualified Stage3.Tree.ExpressionField as Stage3 (Field (Field))
import qualified Stage3.Tree.ExpressionField as Stage3.Field
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import {-# SOURCE #-} Stage4.Temporary.Definition (Definition (Definition))
import {-# SOURCE #-} qualified Stage4.Temporary.Definition as Definition
import Stage4.Temporary.Function (Function (..))
import qualified Stage4.Temporary.Function as Function
import qualified Stage4.Temporary.Pattern as Pattern
import qualified Stage4.Temporary.RightHandSide as RightHandSide
import {-# SOURCE #-} Stage4.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Tree.Declarations as Declarations
import Stage4.Tree.Evidence (Evidence)
import Stage4.Tree.Instanciation (Instanciation (..))
import Stage4.Tree.Statements (Statements)
import qualified Stage4.Tree.Statements as Statements

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
        instanciation :: !(Instanciation scope)
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
      { body :: !(Expression (Scope.Declaration ':+ scope))
      }
  | Call
      { function :: !(Expression scope),
        argument :: !(Expression scope)
      }
  | Join
      { statements :: !(Statements scope)
      }
  deriving (Show)

instance Shift Expression where
  shift = shiftDefault

instance Shift.Functor Expression where
  map = Shift2.mapDefault

instance Shift2.Functor Expression where
  map category
    | general <- Shift2.general category = \case
        Variable {variable, instanciation} ->
          Variable
            { variable = Shift2.map category variable,
              instanciation = Shift.map general instanciation
            }
        Selector {selector, argument} ->
          Selector
            { selector = Shift.map general selector,
              argument = Shift2.map category argument
            }
        Constructor {constructor, arguments} ->
          Constructor
            { constructor = Shift.map general constructor,
              arguments = Shift2.map category <$> arguments
            }
        Method {method, evidence, instanciation} ->
          Method
            { method = Shift.map general method,
              evidence = Shift2.map category evidence,
              instanciation = Shift.map general instanciation
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
            { declarations = Shift2.map (Shift2.Over category) declarations,
              letBody = Shift2.map (Shift2.Over category) letBody
            }
        Lambda {body} ->
          Lambda
            { body = Shift2.map (Shift2.Over category) body
            }
        Call {function, argument} ->
          Call
            { function = Shift2.map category function,
              argument = Shift2.map category argument
            }
        Join {statements} ->
          Join
            { statements = Shift2.map category statements
            }

monoVariable :: Term.Index scope -> Expression scope
monoVariable variable =
  Variable
    { variable,
      instanciation = Instanciation Strict.Vector.empty
    }

call :: Expression scope -> Expression scope -> Expression scope
call function argument = Call {function, argument}

lambdaVariable :: Expression (Scope.Declaration ':+ scopes)
lambdaVariable = monoVariable $ Term.Declaration 0

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

simplifyConstructor ::
  Constructor.Index scope ->
  Int ->
  Int ->
  List (Expression scope) ->
  Expression scope
simplifyConstructor constructor parameters argumentCount arguments
  | parameters > argumentCount =
      Lambda
        { body =
            simplifyConstructor
              (shift constructor)
              parameters
              (argumentCount + 1)
              (fmap shift arguments :> lambdaVariable)
        }
  | parameters == argumentCount =
      Constructor
        { constructor,
          arguments = Strict.Vector.fromList (toList arguments)
        }
  | otherwise = error "bad argument count"

simplifySelector ::
  Selector.Index scope ->
  Selector.Uniform ->
  Expression scope ->
  Expression scope
simplifySelector selector@(Selector.Index typeIndex _) uniform argument = case uniform of
  Selector.Uniform {} -> Selector {selector, argument}
  Selector.Disjoint {indexes} -> Join {statements = mconcat statements}
    where
      generate index select =
        Statements.bind
          ( Pattern.Match
              { irrefutable = False,
                match =
                  Pattern.Constructor
                    { constructor = Constructor.Index typeIndex index,
                      patterns = Strict.Vector.replicate (length indexes) Pattern.Wildcard
                    }
              }
          )
          argument
          ( case select of
              Strict.Just select ->
                Statements.Done
                  { done = monoVariable $ Term.Pattern (Term.Select select Term.At)
                  }
              Strict.Nothing -> Statements.Bottom
          )
      statements = zipWith generate [0 ..] $ toList indexes

simplifyWith ::
  Stage3.Expression scope ->
  [Expression scope] ->
  Expression scope
simplifyWith Stage3.Call {function, argument} arguments =
  simplifyWith function (simplify argument : arguments)
simplifyWith Stage3.Constructor {constructor, parameters} arguments =
  simplifyConstructor constructor parameters (length arguments) (Reverse.fromList arguments)
simplifyWith expression arguments@(_ : _) =
  foldl Call (simplify expression) arguments
simplifyWith expression [] = case expression of
  Stage3.Variable {variable, instanciation} ->
    Variable
      { variable = Term.from variable,
        instanciation
      }
  Stage3.Selector {selector, uniform} ->
    Lambda
      { body = simplifySelector (shift selector) uniform lambdaVariable
      }
  Stage3.Method {method, evidence, instanciation} ->
    Method
      { method,
        evidence,
        instanciation
      }
  Stage3.Record {constructor, parameters, fields} ->
    Constructor
      { constructor,
        arguments = Strict.Vector.generate parameters $ \index' ->
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
      }
  Stage3.Integer {integer, evidence} ->
    Call
      { function =
          Method
            { method = Method.fromInteger,
              evidence,
              instanciation = Instanciation Strict.Vector.empty
            },
        argument = Integer {integer}
      }
  Stage3.Tuple {elements} ->
    Constructor
      { constructor = Constructor.tuple (length elements),
        arguments = simplify <$> Strict.Vector2.toVector elements
      }
  Stage3.List {items} ->
    foldr (cons . simplify) nil items
  Stage3.Let {declarations, letBody} ->
    Let
      { declarations = Declarations.simplify declarations,
        letBody = simplify letBody
      }
  Stage3.If {condition, thenx, elsex} ->
    guard
      ( Statements.bind
          ( Pattern.Match
              { match =
                  Pattern.Constructor
                    { constructor = Constructor.true,
                      patterns = Strict.Vector.empty
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
