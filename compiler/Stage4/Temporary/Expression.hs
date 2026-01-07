module Stage4.Temporary.Expression where

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
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage2.Tree.Selector as Selector (Uniform (..))
import qualified Stage3.Tree.Expression as Stage3 (Expression (..))
import qualified Stage3.Tree.ExpressionField as Stage3 (Field (Field))
import qualified Stage3.Tree.ExpressionField as Stage3.Field
import qualified Stage4.Index.Term as Real.Term
import {-# SOURCE #-} Stage4.Temporary.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Temporary.Declarations as Declarations
import qualified Stage4.Temporary.Pattern as Pattern
import qualified Stage4.Temporary.RightHandSide as RightHandSide
import Stage4.Temporary.Statements (Statements)
import qualified Stage4.Temporary.Statements as Statements
import Stage4.Tree.Evidence (Evidence)
import qualified Stage4.Tree.Expression as Real
import Stage4.Tree.Instanciation (Instanciation (..))

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
  map = Term.mapDefault

instance Term.Functor Expression where
  map category@Term.Category {Term.general} = \case
    Variable {variable, instanciation} ->
      Variable
        { variable = Term.map category variable,
          instanciation = Shift.map general instanciation
        }
    Selector {selector, argument} ->
      Selector
        { selector = Shift.map general selector,
          argument = Term.map category argument
        }
    Constructor {constructor, arguments} ->
      Constructor
        { constructor = Shift.map general constructor,
          arguments = Term.map category <$> arguments
        }
    Method {method, evidence, instanciation} ->
      Method
        { method = Shift.map general method,
          evidence = Term.map category evidence,
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
        { declarations = Term.map (Term.over category) declarations,
          letBody = Term.map (Term.over category) letBody
        }
    Lambda {body} ->
      Lambda
        { body = Term.map (Term.over category) body
        }
    Call {function, argument} ->
      Call
        { function = Term.map category function,
          argument = Term.map category argument
        }
    Join {statements} ->
      Join
        { statements = Term.map category statements
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
      { Statements.left,
        Statements.right = Statements.Done {Statements.done}
      }

simplify :: Stage3.Expression scope -> Expression scope
simplify expression = simplifyWith expression []

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
  Selector.Disjoint {Selector.indexes} -> Join {statements = mconcat statements}
    where
      generate index select =
        Statements.bind
          ( Pattern.Match
              { Pattern.irrefutable = False,
                Pattern.match =
                  Pattern.Constructor
                    { Pattern.constructor = Constructor.Index typeIndex index,
                      Pattern.patterns = Strict.Vector.replicate (length indexes) Pattern.Wildcard
                    }
              }
          )
          argument
          ( case select of
              Strict.Just select ->
                Statements.Done
                  { Statements.done = monoVariable $ Term.Pattern (Term.Select select Term.At)
                  }
              Strict.Nothing -> Statements.Bottom
          )
      statements = zipWith generate [0 ..] $ toList indexes

simplifyWith ::
  Stage3.Expression scope ->
  [Expression scope] ->
  Expression scope
simplifyWith Stage3.Call {Stage3.function, Stage3.argument} arguments =
  simplifyWith function (simplify argument : arguments)
simplifyWith Stage3.Constructor {Stage3.constructor, Stage3.parameters} arguments =
  simplifyConstructor constructor parameters (length arguments) (Reverse.fromList arguments)
simplifyWith expression arguments@(_ : _) =
  foldl Call (simplify expression) arguments
simplifyWith expression [] = case expression of
  Stage3.Variable {Stage3.variable, Stage3.instanciation} ->
    Variable
      { variable,
        instanciation
      }
  Stage3.Selector {Stage3.selector, Stage3.uniform} ->
    Lambda
      { body = simplifySelector (shift selector) uniform lambdaVariable
      }
  Stage3.Method {Stage3.method, Stage3.evidence, Stage3.instanciation} ->
    Method
      { method,
        evidence,
        instanciation
      }
  Stage3.Record {Stage3.constructor, Stage3.parameters, Stage3.fields} ->
    Constructor
      { constructor,
        arguments = Strict.Vector.generate parameters $ \index' ->
          case [ expression
               | Stage3.Field
                   { Stage3.Field.index,
                     Stage3.Field.expression
                   } <-
                   toList fields,
                 index == index'
               ] of
            [] -> Join {statements = Statements.Bottom}
            fields -> simplify $ last fields
      }
  Stage3.Integer {Stage3.integer, Stage3.evidence} ->
    Call
      { function =
          Method
            { method = Method.fromInteger,
              evidence,
              instanciation = Instanciation Strict.Vector.empty
            },
        argument = Integer {integer}
      }
  Stage3.Tuple {Stage3.elements} ->
    Constructor
      { constructor = Constructor.tuple (length elements),
        arguments = simplify <$> Strict.Vector2.toVector elements
      }
  Stage3.List {Stage3.items} ->
    foldr (cons . simplify) nil items
  Stage3.Let {Stage3.declarations, Stage3.letBody} ->
    Let
      { declarations = Declarations.simplify declarations,
        letBody = simplify letBody
      }
  Stage3.If {Stage3.condition, Stage3.thenx, Stage3.elsex} ->
    guard
      ( Statements.bind
          ( Pattern.Match
              { Pattern.match =
                  Pattern.Constructor
                    { Pattern.constructor = Constructor.true,
                      Pattern.patterns = Strict.Vector.empty
                    },
                Pattern.irrefutable = False
              }
          )
          (simplify condition)
          ( Statements.Done
              { Statements.done = shift $ simplify thenx
              }
          )
      )
      (simplify elsex)
  Stage3.MultiwayIf {Stage3.branches} ->
    Join
      { statements =
          foldr1
            Statements.Branch
            (RightHandSide.desugar . RightHandSide.simplify <$> branches)
      }
  Stage3.Character {Stage3.character} ->
    Character {character}
  Stage3.String {Stage3.string} ->
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

finish :: Expression scope -> Real.Expression scope
finish = \case
  Variable {variable, instanciation} ->
    Real.Variable
      { Real.variable = Real.Term.finish variable,
        Real.instanciation
      }
  Constructor {constructor, arguments} ->
    Real.Constructor
      { Real.constructor,
        Real.arguments = finish <$> arguments
      }
  Selector {selector, argument} ->
    Real.Selector
      { Real.selector,
        Real.argument = finish argument
      }
  Method {method, evidence, instanciation} ->
    Real.Method {Real.method, Real.evidence, Real.instanciation}
  Integer {integer} -> Real.Integer {Real.integer}
  Character {character} -> Real.Character {Real.character}
  Let {declarations, letBody} ->
    Real.Let
      { Real.declarations = Declarations.finish declarations,
        Real.letBody = finish letBody
      }
  Lambda {body} ->
    Real.Lambda
      { Real.body = finish body
      }
  Call {function, argument} ->
    Real.Call
      { Real.function = finish function,
        Real.argument = finish argument
      }
  Join {statements} ->
    Real.Join
      { Real.statements = Statements.finish statements
      }
