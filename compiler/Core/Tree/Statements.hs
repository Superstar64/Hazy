module Core.Tree.Statements where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Temporary.Pattern (Pattern)
import qualified Core.Temporary.Pattern as Pattern
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} Core.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Core.Tree.Declarations as Declarations
import Core.Tree.EntryInfo (EntryInfo (..))
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Type as Type
import Data.Foldable (toList)
import Data.Strict.Vector1 as Strict.Vector1 (fromList', uncons)
import Data.Text (unpack)
import Data.Vector.Strict ((//))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.Simple.ConstructorInfo as Semantic (ConstructorInfo (..))
import qualified Semantic.Check.Simple.ConstructorInfo as Semantic.ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Constructor as Constructor2
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Statements as Semantic (Guard, Statements (..))

data Statements scope
  = Done {done :: !(Expression scope)}
  | Bind
      { constructor :: !(Constructor.Index scope),
        constructorInfo :: !(ConstructorInfo scope),
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
    Bind {constructor, constructorInfo, check, thenx} ->
      Bind
        { constructor = Substitute.map category constructor,
          constructorInfo = Substitute.map category constructorInfo,
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
  Pattern.Constructor {constructor, patterns, constructorInfo} -> go (length patterns - 1)
    where
      -- todo this is `O(n^2)`
      go -1
        | patternx <- Pattern.Match {match, irrefutable} =
            expand patternx check thenx
      go index = case patterns Strict.Vector.! index of
        Pattern.Wildcard -> go (index - 1)
        target
          | patterns <- patterns // [(index, Pattern.Wildcard)],
            match <- Pattern.Constructor {constructor, patterns, constructorInfo},
            patternx <- Pattern.Match {match, irrefutable},
            target <- shift target,
            check' <- Expression.monoVariable $ Term.Pattern $ Term.Select index Term.At,
            thenx <- Shift2.map (Shift2.SimplifyPattern index) thenx ->
              bind patternx check (bind target check' thenx)
  Pattern.Record {constructor, fields, constructorInfo = constructorInfo} ->
    go (length fields - 1)
    where
      go -1
        | entryCount <- Semantic.ConstructorInfo.entryCount constructorInfo,
          patterns <- Strict.Vector.replicate entryCount Pattern.Wildcard,
          match <- Pattern.Constructor {constructor, patterns, constructorInfo},
          patternx <- Pattern.Match {match, irrefutable},
          fields <- (\(Pattern.Field field _) -> field) <$> fields,
          thenx <- Shift2.map (Shift2.RenamePattern (fields Strict.Vector.!)) thenx =
            expand patternx check thenx
      go index = case fields Strict.Vector.! index of
        Pattern.Field field patternx -> case patternx of
          Pattern.Wildcard -> go (index - 1)
          target
            | fields <- fields // [(index, Pattern.Field field Pattern.Wildcard)],
              match <- Pattern.Record {constructor, fields, constructorInfo},
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
              patterns = Strict.Vector.empty,
              constructorInfo = Semantic.ConstructorInfo {entries = Strict.Vector.empty}
            },
      tail <- Pattern.Match {match, irrefutable},
      patterns <- Strict.Vector.fromList [head, tail],
      match <-
        Pattern.Constructor
          { constructor = Constructor.cons,
            patterns,
            constructorInfo =
              Semantic.ConstructorInfo
                { entries =
                    Strict.Vector.fromList
                      [ EntryInfo {strict = Type.Constructor Type2.Lazy},
                        EntryInfo {strict = Type.Constructor Type2.Lazy}
                      ]
                }
          },
      cons <- Pattern.Match {match, irrefutable} ->
        bind cons check (Shift2.map Shift2.SimplifyList thenx)
  Pattern.String {string}
    | let wrap character
            | match <- Pattern.Character {character} =
                Pattern.Match {match, irrefutable},
      characters <- map wrap $ unpack string,
      match <- case characters of
        (head : tail) -> Pattern.List {items = Strict.Vector1.fromList' head tail}
        [] ->
          Pattern.Constructor
            { constructor = Constructor.nil,
              patterns = Strict.Vector.empty,
              constructorInfo = Semantic.ConstructorInfo {entries = Strict.Vector.empty}
            },
      patternx <- Pattern.Match {match, irrefutable} ->
        bind patternx check thenx
  Pattern.Character {character} ->
    bind Pattern.Wildcard check $
      bind true equal (shift thenx)
    where
      equal = Expression.eqChar Expression.patternVariable (Expression.character_ character)
  Pattern.Integer {integer, evidence, equal} ->
    bind Pattern.Wildcard check $
      bind true equal' (shift thenx)
    where
      equal' =
        Expression.eq
          (shift equal)
          Expression.patternVariable
          (Expression.integer_ integer $ shift evidence)
  Pattern.Float {float, evidence, equal} ->
    bind Pattern.Wildcard check $
      bind true equal' (shift thenx)
    where
      equal' =
        Expression.eq
          (shift equal)
          Expression.patternVariable
          (Expression.float_ float $ shift evidence)
  where
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
    replace ::
      Constructor2.Index scope ->
      ConstructorInfo scope ->
      Expression scope ->
      Statements (Scope.SimplePattern ':+ scope) ->
      Int ->
      Statements scope
    replace _ _ _ thenx -1 = Shift2.map (Shift2.Lift $ Shift.Unshift fail) thenx
      where
        fail = error "bad irrefutable replace"
    replace constructor constructorInfo check thenx n =
      LetOne
        { declaration =
            Expression.join_
              Bind
                { constructor,
                  constructorInfo,
                  check,
                  thenx =
                    Done
                      { done = Expression.patternVariableAt n
                      }
                },
          body =
            replace
              (shift constructor)
              (shift constructorInfo)
              (shift check)
              (Shift2.map (Shift2.ReplaceIrrefutable n) thenx)
              (n - 1)
        }
    finish ::
      forall scope.
      Pattern scope ->
      Expression scope ->
      Statements (Scope.Pattern ':+ scope) ->
      Statements scope
    finish Pattern.Match {match, irrefutable} check thenx
      | Pattern.Constructor {constructor, patterns, constructorInfo} <- match,
        all wildcard patterns,
        thenx <- Shift2.map Shift2.FinishPattern thenx =
          case constructorInfo of
            Semantic.Newtype ->
              LetOne
                { declaration = Expression.newtype_ constructor check Expression.Destruct,
                  body = Shift2.map Shift2.FinishNewtype thenx
                }
            Semantic.ConstructorInfo {entries}
              | irrefutable -> replace constructor constructorInfo check thenx (length patterns - 1)
              | otherwise -> Bind {constructor, constructorInfo, check, thenx}
              where
                constructorInfo = ConstructorInfo {entries}
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
              patterns = Strict.Vector.empty,
              constructorInfo = Semantic.ConstructorInfo {entries = Strict.Vector.empty}
            },
        irrefutable = False
      }
  )

simplify :: Semantic.Statements Semantic.Guard Normal Check scope -> Statements scope
simplify = \case
  Semantic.Done {done} -> Done {done = Expression.simplify done}
  Semantic.Run {effect, after} ->
    bind
      true
      (Expression.simplify effect)
      (shift $ simplify after)
  Semantic.Bind {patternx, effect, thenx} ->
    bind
      (Pattern.simplify patternx)
      (Expression.simplify effect)
      (simplify thenx)
  Semantic.Let {declarations, body} ->
    Let
      { declarations = Declarations.simplify declarations,
        letBody = simplify body
      }

call :: Statements scope -> Expression scope -> Statements scope
call statements argument = case statements of
  Done {done} -> Done {done = Expression.call done argument}
  Bind {constructor, constructorInfo, check, thenx} ->
    Bind
      { constructor,
        constructorInfo,
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
