module Stage4.Tree.Statements where

import Data.Foldable (toList)
import Data.Strict.Vector1 as Strict.Vector1 (fromList', uncons)
import Data.Text (unpack)
import Data.Vector.Strict ((//))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Constructor as Constructor2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Statements as Stage3
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Temporary.Pattern (Pattern)
import qualified Stage4.Temporary.Pattern as Pattern
import {-# SOURCE #-} Stage4.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Expression

data Statements scope
  = Done {done :: !(Expression scope)}
  | Bind
      { constructor :: !(Constructor.Index scope),
        patterns :: !Int,
        check :: !(Expression scope),
        thenx :: !(Statements (Scope.SimplePattern ':+ scope))
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Statements (Scope.Declaration ':+ scope))
      }
  | LetOne
      { declaration :: !(Expression scope),
        body :: !(Statements (Scope.SimpleDeclaration ':+ scope))
      }
  | Branch
      { left :: !(Statements scope),
        right :: !(Statements scope)
      }
  | Bottom
  deriving (Show)

instance Semigroup (Statements scope) where
  (<>) = Branch

instance Monoid (Statements scope) where
  mempty = Bottom

instance Shift Statements where
  shift = shiftDefault

instance Shift.Functor Statements where
  map = Shift2.mapDefault

instance Shift2.Functor Statements where
  map = Substitute.mapDefault

instance Substitute.Functor Statements where
  map category = \case
    Done {done} ->
      Done
        { done = Substitute.map category done
        }
    Bind {constructor, patterns, check, thenx} ->
      Bind
        { constructor = Substitute.map category constructor,
          patterns,
          check = Substitute.map category check,
          thenx = Substitute.map (Substitute.Over category) thenx
        }
    LetOne {declaration, body} ->
      LetOne
        { declaration = Substitute.map category declaration,
          body = Substitute.map (Substitute.Over category) body
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Substitute.map (Substitute.Over category) declarations,
          letBody = Substitute.map (Substitute.Over category) letBody
        }
    Branch {left, right} ->
      Branch
        { left = Substitute.map category left,
          right = Substitute.map category right
        }
    Bottom -> Bottom

bind :: Pattern scope -> Expression scope -> Statements (Scope.Pattern ':+ scope) -> Statements scope
bind Pattern.Wildcard check thenx =
  LetOne
    { declaration = check,
      body = Shift2.map Shift2.ReplaceWildcard thenx
    }
bind Pattern.Match {match, irrefutable} check thenx = case match of
  Pattern.Constructor {constructor, patterns} -> go (length patterns - 1)
    where
      -- todo this is `O(n^2)`
      go -1
        | patternx <- Pattern.Match {match, irrefutable} =
            expand patternx check thenx
      go index = case patterns Strict.Vector.! index of
        Pattern.Wildcard -> go (index - 1)
        target
          | patterns <- patterns // [(index, Pattern.Wildcard)],
            match <- Pattern.Constructor {constructor, patterns},
            patternx <- Pattern.Match {match, irrefutable},
            target <- shift target,
            check' <- Expression.monoVariable $ Term.Pattern $ Term.Select index Term.At,
            thenx <- Shift2.map (Shift2.SimplifyPattern index) thenx ->
              bind patternx check (bind target check' thenx)
  Pattern.Record {constructor, fields, fieldCount} -> go (length fields - 1)
    where
      go -1
        | patterns <- Strict.Vector.replicate fieldCount Pattern.Wildcard,
          match <- Pattern.Constructor {constructor, patterns},
          patternx <- Pattern.Match {match, irrefutable},
          fields <- (\(Pattern.Field field _) -> field) <$> fields,
          thenx <- Shift2.map (Shift2.RenamePattern (fields Strict.Vector.!)) thenx =
            expand patternx check thenx
      go index = case fields Strict.Vector.! index of
        Pattern.Field field patternx -> case patternx of
          Pattern.Wildcard -> go (index - 1)
          target
            | fields <- fields // [(index, Pattern.Field field Pattern.Wildcard)],
              match <- Pattern.Record {constructor, fields, fieldCount},
              patternx <- Pattern.Match {match, irrefutable},
              target <- shift target,
              check' <- Expression.monoVariable $ Term.Pattern $ Term.Select index Term.At,
              thenx <- Shift2.map (Shift2.SimplifyPattern index) thenx ->
                bind patternx check (bind target check' thenx)
  Pattern.List {items}
    | (head, items) <- Strict.Vector1.uncons items,
      match <- case Strict.Vector.uncons items of
        -- todo this is inefficent
        Just (head, tail) -> Pattern.List {items = Strict.Vector1.fromList' head (toList tail)}
        Nothing ->
          Pattern.Constructor
            { constructor = Constructor.nil,
              patterns = Strict.Vector.empty
            },
      tail <- Pattern.Match {match, irrefutable},
      patterns <- Strict.Vector.fromList [head, tail],
      match <-
        Pattern.Constructor
          { constructor = Constructor.cons,
            patterns
          },
      cons <- Pattern.Match {match, irrefutable} ->
        bind cons check (Shift2.map Shift2.SimplifyList thenx)
  Pattern.String {text}
    | let wrap character
            | match <- Pattern.Character {character} =
                Pattern.Match {match, irrefutable},
      characters <- map wrap $ unpack text,
      match <- case characters of
        (head : tail) -> Pattern.List {items = Strict.Vector1.fromList' head tail}
        [] ->
          Pattern.Constructor
            { constructor = Constructor.nil,
              patterns = Strict.Vector.empty
            },
      patternx <- Pattern.Match {match, irrefutable} ->
        bind patternx check thenx
  Pattern.Character {character} ->
    -- expand patternx check thenx
    bind Pattern.Wildcard check $
      bind true equal (shift thenx)
    where
      equal = Expression.eqChar Expression.patternVariable (Expression.character_ character)
  where
    -- todo deal with irrefutable cases
    expand ::
      Pattern scope ->
      Expression scope ->
      Statements (Scope.Pattern ':+ scope) ->
      Statements scope
    expand patternx check thenx =
      LetOne
        { declaration = check,
          body =
            finish
              (shift patternx)
              Expression.lambdaVariable
              (Shift2.map Shift2.LetPattern thenx)
        }
    finish ::
      Pattern scope ->
      Expression scope ->
      Statements (Scope.Pattern ':+ scope) ->
      Statements scope
    finish Pattern.Match {match} check thenx
      | Pattern.Constructor {constructor, patterns} <- match,
        all wildcard patterns =
          Bind
            { constructor,
              patterns = length patterns,
              check,
              thenx = Shift2.map Shift2.FinishPattern thenx
            }
      where
        wildcard Pattern.Wildcard {} = True
        wildcard _ = False
    finish _ _ _ = error "bad finish"

true :: Pattern scope
true =
  ( Pattern.Match
      { match =
          Pattern.Constructor
            { constructor = Constructor2.true,
              patterns = Strict.Vector.empty
            },
        irrefutable = False
      }
  )

simplify :: Stage3.Statements scope -> Statements scope
simplify = \case
  Stage3.Done {done} -> Done {done = Expression.simplify done}
  Stage3.Run {check, after} ->
    bind
      true
      (Expression.simplify check)
      (shift $ simplify after)
  Stage3.Bind {patternx, check, thenx} ->
    bind
      (Pattern.simplify patternx)
      (Expression.simplify check)
      (simplify thenx)
  Stage3.Let {declarations, body} ->
    Let
      { declarations = Declarations.simplify declarations,
        letBody = simplify body
      }

call :: Statements scope -> Expression scope -> Statements scope
call statements argument = case statements of
  Done {done} -> Done {done = Expression.call done argument}
  Bind {constructor, patterns, check, thenx} ->
    Bind
      { constructor,
        patterns,
        check,
        thenx = call thenx (shift argument)
      }
  Let {declarations, letBody} ->
    Let
      { declarations,
        letBody = call letBody (shift argument)
      }
  LetOne {declaration, body} ->
    LetOne
      { declaration,
        body = call body (shift argument)
      }
  Branch {left, right} ->
    Branch
      { left = call left argument,
        right = call right argument
      }
  Bottom -> Bottom
