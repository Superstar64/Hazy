module Stage4.Temporary.Statements where

import Data.Foldable (toList)
import Data.Strict.Vector1 as Strict.Vector1 (fromList', uncons)
import Data.Text (unpack)
import Data.Vector.Strict ((//))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Constructor as Constructor2
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Statements as Stage3
import {-# SOURCE #-} Stage4.Temporary.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Temporary.Declarations as Declarations
import {-# SOURCE #-} Stage4.Temporary.Expression (Expression)
import {-# SOURCE #-} qualified Stage4.Temporary.Expression as Expression
import Stage4.Temporary.Pattern (Pattern)
import qualified Stage4.Temporary.Pattern as Pattern
import qualified Stage4.Tree.Pattern as Real.Pattern
import qualified Stage4.Tree.Statements as Real

data Statements scope
  = Done {done :: !(Expression scope)}
  | Bind
      { patternx :: !(Real.Pattern.Pattern scope),
        check :: !(Expression scope),
        thenx :: !(Statements (Scope.Pattern ':+ scope))
      }
  | Let
      { declarations :: !(Declarations (Scope.Declaration ':+ scope)),
        letBody :: !(Statements (Scope.Declaration ':+ scope))
      }
  | LetOne
      { declaration :: !(Expression scope),
        body :: !(Statements (Scope.Declaration ':+ scope))
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
  map = Term.mapDefault

instance Term.Functor Statements where
  map category = \case
    Done {done} ->
      Done
        { done = Term.map category done
        }
    Bind {patternx, check, thenx} ->
      Bind
        { patternx = Term.map category patternx,
          check = Term.map category check,
          thenx = Term.map (Term.over category) thenx
        }
    LetOne {declaration, body} ->
      LetOne
        { declaration = Term.map category declaration,
          body = Term.map (Term.over category) body
        }
    Let {declarations, letBody} ->
      Let
        { declarations = Term.map (Term.over category) declarations,
          letBody = Term.map (Term.over category) letBody
        }
    Branch {left, right} ->
      Branch
        { left = Term.map category left,
          right = Term.map category right
        }
    Bottom -> Bottom

bind :: Pattern scope -> Expression scope -> Statements (Scope.Pattern ':+ scope) -> Statements scope
bind Pattern.Wildcard check thenx =
  LetOne
    { declaration = check,
      body =
        let category ::
              Term.Category
                (Scope.Pattern ':+ scope)
                (Scope.Declaration ':+ scope)
            category =
              Term.Category
                { general =
                    Shift.Unshift (error "bad unshift")
                      Shift.:. Shift.Over Shift.Shift,
                  term = \case
                    Term.Pattern Term.At -> Term.Declaration 0
                    Term.Shift index -> Term.Shift index
                    _ -> error "bad wildcard bind"
                }
         in Term.map category thenx
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
            let category =
                  Term.Category
                    { general = Shift.Shift,
                      term = \case
                        Term.Pattern (Term.Select index' bound)
                          | index == index' -> Term.Pattern bound
                        index -> shift index
                    },
            thenx <- Term.map category thenx ->
              bind patternx check (bind target check' thenx)
  Pattern.Record {constructor, fields, fieldCount} -> go (length fields - 1)
    where
      go -1
        | patterns <- Strict.Vector.replicate fieldCount Pattern.Wildcard,
          match <- Pattern.Constructor {constructor, patterns},
          patternx <- Pattern.Match {match, irrefutable},
          fields <- (\(Pattern.Field field _) -> field) <$> fields,
          let category :: Term.Category (Scope.Pattern ':+ scope) (Scope.Pattern ':+ scope)
              category =
                Term.Category
                  { general = Shift.Id,
                    term = \case
                      Term.Pattern Term.At -> Term.Pattern Term.At
                      Term.Pattern (Term.Select index' bound) ->
                        Term.Pattern (Term.Select (fields Strict.Vector.! index') bound)
                      Term.Shift index -> Term.Shift index
                  },
          thenx <- Term.map category thenx =
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
              let category =
                    Term.Category
                      { general = Shift.Shift,
                        term = \case
                          Term.Pattern (Term.Select index' bound)
                            | index == index' -> Term.Pattern bound
                          index -> shift index
                      },
              thenx <- Term.map category thenx ->
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
      cons <- Pattern.Match {match, irrefutable},
      let category :: Term.Category (Scope.Pattern ':+ scope) (Scope.Pattern ':+ scope)
          category =
            Term.Category
              { general = Shift.Id,
                term = \case
                  Term.Pattern Term.At -> Term.Pattern Term.At
                  Term.Pattern (Term.Select 0 bound) -> Term.Pattern (Term.Select 0 bound)
                  Term.Pattern (Term.Select index bound) ->
                    Term.Pattern (Term.Select 1 (Term.Select (index - 1) bound))
                  Term.Shift index -> Term.Shift index
              } ->
        bind cons check (Term.map category thenx)
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
  Pattern.Character {}
    | patternx <- Pattern.Match {match, irrefutable} ->
        expand patternx check thenx
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
              ( let category ::
                      Term.Category
                        (Scope.Pattern ':+ scope)
                        (Scope.Pattern ':+ (Scope.Declaration ':+ scope))
                    category =
                      Term.Category
                        { general = Shift.Over Shift.Shift,
                          term = \case
                            Term.Pattern Term.At -> Term.Shift $ Term.Declaration 0
                            Term.Pattern (Term.Select index bound) ->
                              Term.Pattern (Term.Select index bound)
                            Term.Shift index -> Term.Shift (Term.Shift index)
                        }
                 in Term.map category thenx
              )
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
            { patternx =
                Real.Pattern.Constructor
                  { constructor,
                    patterns = length patterns
                  },
              check,
              thenx
            }
      | Pattern.Character {character} <- match =
          Bind
            { patternx = Real.Pattern.Character {character},
              check,
              thenx
            }
      where
        wildcard Pattern.Wildcard {} = True
        wildcard _ = False
    finish _ _ _ = error "bad finish"

simplify :: Stage3.Statements scope -> Statements scope
simplify = \case
  Stage3.Done {done} -> Done {done = Expression.simplify done}
  Stage3.Run {check, after} ->
    bind
      ( Pattern.Match
          { match =
              Pattern.Constructor
                { constructor = Constructor2.true,
                  patterns = Strict.Vector.empty
                },
            irrefutable = False
          }
      )
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
  Bind {patternx, check, thenx} ->
    Bind
      { patternx,
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

finish :: Statements scope -> Real.Statements scope
finish = \case
  Done {done} ->
    Real.Done
      { done = Expression.finish done
      }
  Bind {patternx, check, thenx} ->
    Real.Bind
      { patternx,
        check = Expression.finish check,
        thenx = finish thenx
      }
  Let {declarations, letBody} ->
    Real.Let
      { declarations = Declarations.finish declarations,
        letBody = finish letBody
      }
  LetOne {declaration, body} ->
    Real.LetOne
      { declaration = Expression.finish declaration,
        body = finish body
      }
  Branch {left, right} ->
    Real.Branch
      { left = finish left,
        right = finish right
      }
  Bottom -> Real.Bottom
