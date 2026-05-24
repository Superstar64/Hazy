module Stage2.Tree.Statements where

import Data.Foldable (toList)
import Data.Kind (Type)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import qualified Stage1.Tree.Statement as Stage1 (Statement (..))
import qualified Stage1.Tree.Statements as Stage1 (Statements (..))
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import Stage2.Layout (Normal)
import qualified Stage2.Locality as Locality
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, neverFails, resolve)
import qualified Stage4.Tree.Evidence as Simple (Evidence)

data Syntax
  = Guard
  | Do

type Guard = 'Guard

type Do = 'Do

type Equal :: Syntax -> Syntax -> Type
data Equal syntax1 syntax2 where
  Refl :: Equal syntax syntax

class IsDo syntax where
  isDo :: Equal Do syntax

instance IsDo 'Do where
  isDo = Refl

data Statements syntax layout stage scope
  = Done
      { done :: !(Expression layout stage scope)
      }
  | Run
      { startPosition :: !Position,
        evidence :: !(Inferred (Evidence syntax) stage scope),
        effect :: !(Expression layout stage scope),
        after :: !(Statements syntax layout stage scope)
      }
  | Bind
      { startPosition :: !Position,
        evidence :: !(Inferred (Evidence syntax) stage scope),
        patternx :: !(Pattern stage scope),
        effect :: !(Expression layout stage scope),
        thenx :: !(Statements syntax layout stage (Scope.Pattern ':+ scope)),
        fail :: !Bool
      }
  | Let
      { startPosition :: !Position,
        declarations :: !(Declarations Locality.Local layout stage (Scope.Declaration ':+ scope)),
        body :: !(Statements syntax layout stage (Scope.Declaration ':+ scope))
      }
  deriving (Show)

instance Shift (Statements syntax layout stage) where
  shift = shiftDefault

instance Shift.Functor (Statements syntax layout stage) where
  map category = \case
    Done {done} ->
      Done
        { done = Shift.map category done
        }
    Run {startPosition, evidence, effect, after} ->
      Run
        { startPosition,
          evidence = Shift.map category evidence,
          effect = Shift.map category effect,
          after = Shift.map category after
        }
    Bind {startPosition, evidence, patternx, effect, thenx, fail} ->
      Bind
        { startPosition,
          evidence = Shift.map category evidence,
          patternx = Shift.map category patternx,
          effect = Shift.map category effect,
          thenx = Shift.map (Shift.Over category) thenx,
          fail
        }
    Let {startPosition, declarations, body} ->
      Let
        { startPosition,
          declarations = Shift.map (Shift.Over category) declarations,
          body = Shift.map (Shift.Over category) body
        }

instance FreeTermVariables (Statements syntax layout) where
  freeTermVariables target = \case
    Done {done} -> freeTermVariables target done
    Run {effect, after} ->
      concat
        [ freeTermVariables target effect,
          freeTermVariables target after
        ]
    Bind {effect, thenx} ->
      concat
        [ freeTermVariables target effect,
          freeTermVariables (FreeVariables.Over target) thenx
        ]
    Let {declarations, body} ->
      concat
        [ freeTermVariables (FreeVariables.Over target) declarations,
          freeTermVariables (FreeVariables.Over target) body
        ]

instance Connect (Statements syntax) where
  connect = \case
    Done {done} ->
      Done
        { done = connect done
        }
    Run {startPosition, effect, after} ->
      Run
        { startPosition,
          evidence = Inferred,
          effect = connect effect,
          after = connect after
        }
    Bind {startPosition, patternx, effect, thenx, fail} ->
      Bind
        { startPosition,
          evidence = Inferred,
          patternx,
          effect = connect effect,
          thenx = connect thenx,
          fail
        }
    Let {startPosition, declarations, body} ->
      Let
        { startPosition,
          declarations = Declarations.connect declarations,
          body = connect body
        }
  seperate = \case
    Done {done} ->
      Done
        { done = seperate done
        }
    Run {startPosition, evidence, effect, after} ->
      Run
        { startPosition,
          evidence,
          effect = seperate effect,
          after = seperate after
        }
    Bind {startPosition, evidence, patternx, effect, thenx, fail} ->
      Bind
        { startPosition,
          evidence,
          patternx,
          effect = seperate effect,
          thenx = seperate thenx,
          fail
        }
    Let {startPosition, declarations, body} ->
      Let
        { startPosition,
          declarations = Declarations.seperate declarations,
          body = seperate body
        }

data Evidence syntax scope where
  Bool :: Evidence Guard scope
  Monad :: !(Simple.Evidence scope) -> Evidence Do scope

instance Show (Evidence syntax scope) where
  showsPrec d = \case
    Bool -> showString "Bool"
    Monad evidence -> showParen (d > 10) $ showString "Monad " . showsPrec 11 evidence

instance Scope.Show (Evidence syntax) where
  showsPrec = showsPrec

instance Shift (Evidence syntax) where
  shift = shiftDefault

instance Shift.Functor (Evidence syntax) where
  map category = \case
    Bool -> Bool
    Monad evidence -> Monad (Shift.map category evidence)

resolve :: Context scope -> Stage1.Statements Position -> Statements syntax Normal Resolve scope
resolve context Stage1.Statements {body, done} = statements context (toList body) done
  where
    statements ::
      Context scope ->
      [Stage1.Statement Position] ->
      Stage1.Expression Position ->
      Statements syntax Normal Resolve scope
    statements context [] done =
      Done
        { done = Expression.resolve context done
        }
    statements context (Stage1.Run {startPosition, expression} : remaining) done =
      Run
        { startPosition,
          evidence = Inferred,
          effect = Expression.resolve context expression,
          after = statements context remaining done
        }
    statements context (Stage1.Bind {startPosition, patternx, expression} : remaining) done =
      Bind
        { startPosition,
          evidence = Inferred,
          patternx = pattern',
          effect = Expression.resolve context expression,
          thenx = statements (Pattern.augment pattern' context) remaining done,
          fail = not $ Pattern.neverFails pattern'
        }
      where
        pattern' = Pattern.resolve context patternx
    statements context (Stage1.Let {startPosition, declarations} : remaining) done
      | (context, declarations) <- Declarations.resolve context declarations =
          Let
            { startPosition,
              declarations,
              body = statements context remaining done
            }
