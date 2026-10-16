module Core.Tree.Statements where

import qualified Core.Substitute as Substitute
import Core.Tree.ConstructorInfo (ConstructorInfo (..))
import {-# SOURCE #-} Core.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Core.Tree.Declarations as Declarations
import Core.Tree.EntryInfo (EntryInfo (..))
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Core.Tree.Expression as Expression
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Strict.Vector1 as Strict.Vector1 (fromList', uncons)
import Data.Vector.Strict ((//))
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.Info.Constructor as Semantic (ConstructorInfo (..))
import qualified Semantic.Check.Info.Constructor as Semantic.ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Constructor as Constructor2
import qualified Semantic.Index.Term as Term
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Declarations as Semantic.Declarations
import Semantic.Tree.Pattern (Failable (Unused), Pattern (..))
import qualified Semantic.Tree.Pattern as Pattern
import qualified Semantic.Tree.Statements as Semantic (Guard, Statements (..))
import qualified Syntax.StringLiteral as StringLiteral

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
      { levity :: !(Type scope),
        declaration :: !(Expression scope),
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

instance Shift0.Functor Statements where
  map = Shift.mapDefault

instance Shift.Functor Statements where
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
    LetOne {levity, declaration, body} ->
      LetOne
        { levity = Substitute.map category levity,
          declaration = Substitute.map category declaration,
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

bind :: Pattern () Check scope -> Expression scope -> Statements (Scope.Pattern ':+ scope) -> Statements scope
bind Pattern.Wildcard {levity = Solved levity} check thenx =
  LetOne
    { levity,
      declaration = check,
      body = Shift.map Shift.ReplaceWildcard thenx
    }
bind Pattern.Match {levity = Solved levity, match, irrefutable} check thenx = case match of
  Pattern.Constructor {constructor, patterns, constructorInfo = Solved constructorInfo} ->
    go (length patterns - 1)
    where
      -- todo this is `O(n^2)`
      go (-1) = expand irrefutable levity constructor patterns constructorInfo check thenx
      go index = case patterns Strict.Vector.! index of
        Pattern.Wildcard {} -> go (index - 1)
        target@Pattern.Match {levity = Solved innerLevity}
          | patterns <-
              patterns
                // [ ( index,
                       Pattern.Wildcard
                         { startPosition = (),
                           levity = Solved innerLevity,
                           names = Map.empty
                         }
                     )
                   ],
            match <-
              Pattern.Constructor
                { constructorPosition = (),
                  constructor,
                  patterns,
                  constructorInfo = Solved constructorInfo,
                  failable = Unused
                },
            patternx <-
              Pattern.Match
                { startPosition = (),
                  levity = Solved levity,
                  names = Map.empty,
                  match,
                  irrefutable
                },
            target <- shift target,
            variable <- Term.Pattern $ Term.Select index Term.At,
            check' <- Expression.monoVariable (shift innerLevity) variable,
            thenx <- Shift.map (Shift.SimplifyPattern index) thenx ->
              bind patternx check (bind target check' thenx)
  Pattern.Record {constructor, fields, constructorInfo = Solved constructorInfo} ->
    go (length fields - 1)
    where
      go (-1)
        | entryCount <- Semantic.ConstructorInfo.entryCount constructorInfo,
          indexes <- (\Pattern.Field {index} -> index) <$> fields,
          patterns <-
            Strict.Vector.generate entryCount $ \index ->
              Pattern.Wildcard
                { startPosition = (),
                  levity = Solved $ case constructorInfo of
                    Semantic.ConstructorInfo {entries}
                      | EntryInfo {levity} <- entries Strict.Vector.! index -> levity
                    Semantic.Newtype {} -> Type.lazy,
                  names = Map.empty
                },
          thenx <- Shift.map (Shift.RenamePattern (indexes Strict.Vector.!)) thenx =
            expand irrefutable levity constructor patterns constructorInfo check thenx
      go index = case fields Strict.Vector.! index of
        Pattern.Field {index = field, field = patternx} -> case patternx of
          Pattern.Wildcard {} -> go (index - 1)
          target@Pattern.Match {levity = Solved innerLevity}
            | fields <-
                fields
                  // [ ( index,
                         Pattern.Field
                           { index = field,
                             field =
                               Pattern.Wildcard
                                 { startPosition = (),
                                   levity = Solved innerLevity,
                                   names = Map.empty
                                 }
                           }
                       )
                     ],
              match <-
                Pattern.Record
                  { constructorPosition = (),
                    constructor,
                    fields,
                    constructorInfo = Solved constructorInfo,
                    failable = Unused
                  },
              patternx <-
                Pattern.Match
                  { startPosition = (),
                    levity = Solved levity,
                    match,
                    irrefutable,
                    names = Map.empty
                  },
              target <- shift target,
              variable <- Term.Pattern $ Term.Select index Term.At,
              check' <- Expression.monoVariable (shift innerLevity) variable,
              thenx <- Shift.map (Shift.SimplifyPattern index) thenx ->
                bind patternx check (bind target check' thenx)
  Pattern.List {items}
    | (head, items) <- Strict.Vector1.uncons items,
      match <- case Strict.Vector.uncons items of
        -- todo this is inefficent
        Just (head, tail) ->
          Pattern.List
            { items = Strict.Vector1.fromList' head (toList tail)
            }
        Nothing ->
          Pattern.Constructor
            { constructorPosition = (),
              constructor = Constructor.nil,
              patterns = Strict.Vector.empty,
              constructorInfo = Solved Semantic.ConstructorInfo {entries = Strict.Vector.empty},
              failable = Unused
            },
      tail <-
        Pattern.Match
          { startPosition = (),
            levity = Solved Type.lazy,
            match,
            irrefutable,
            names = Map.empty
          },
      patterns <- Strict.Vector.fromList [head, tail],
      match <-
        Pattern.Constructor
          { constructorPosition = (),
            constructor = Constructor.cons,
            patterns,
            constructorInfo =
              Solved
                Semantic.ConstructorInfo
                  { entries =
                      Strict.Vector.fromList
                        [ EntryInfo {levity = Type.lazy},
                          EntryInfo {levity = Type.lazy}
                        ]
                  },
            failable = Unused
          },
      cons <-
        Pattern.Match
          { startPosition = (),
            levity = Solved levity,
            match,
            irrefutable,
            names = Map.empty
          } ->
        bind cons check (Shift.map Shift.SimplifyList thenx)
  Pattern.String {string}
    | let wrap character
            | match <- Pattern.Character {character} =
                Pattern.Match
                  { startPosition = (),
                    levity = Solved Type.lazy,
                    match,
                    irrefutable = False,
                    names = Map.empty
                  },
      characters <- map wrap $ StringLiteral.unpack string,
      match <- case characters of
        (head : tail) ->
          Pattern.List
            { items = Strict.Vector1.fromList' head tail
            }
        [] ->
          Pattern.Constructor
            { constructorPosition = (),
              constructor = Constructor.nil,
              patterns = Strict.Vector.empty,
              constructorInfo = Solved Semantic.ConstructorInfo {entries = Strict.Vector.empty},
              failable = Unused
            },
      patternx <-
        Pattern.Match
          { startPosition = (),
            levity = Solved levity,
            match,
            irrefutable,
            names = Map.empty
          } ->
        bind patternx check thenx
  Pattern.Character {character} ->
    bind
      Pattern.Wildcard
        { startPosition = (),
          levity = Solved levity,
          names = Map.empty
        }
      check
      $ bind true equal (shift thenx)
    where
      equal =
        Expression.eqChar
          (Expression.patternVariable levity)
          (Expression.character_ character)
  Pattern.Integer {integer, evidence = Solved evidence, equal = Solved equal} ->
    bind
      Pattern.Wildcard
        { startPosition = (),
          levity = Solved levity,
          names = Map.empty
        }
      check
      $ bind true equal' (shift thenx)
    where
      equal' =
        Expression.eq
          (shift equal)
          (Expression.patternVariable levity)
          (Expression.integer_ integer $ shift evidence)
  Pattern.Float {float, evidence = Solved evidence, equal = Solved equal} ->
    bind
      Pattern.Wildcard
        { startPosition = (),
          levity = Solved levity,
          names = Map.empty
        }
      check
      $ bind true equal' (shift thenx)
    where
      equal' =
        Expression.eq
          (shift equal)
          (Expression.patternVariable levity)
          (Expression.float_ float $ shift evidence)

expand ::
  Bool ->
  Type scope ->
  Constructor.Index scope ->
  Strict.Vector (Pattern position Check scope) ->
  Semantic.ConstructorInfo scope ->
  Expression scope ->
  Statements (Scope.Pattern ':+ scope) ->
  Statements scope
expand irrefutable levity constructor patterns constructorInfo check thenx =
  LetOne
    { levity,
      declaration = check,
      body =
        finish
          irrefutable
          (shift levity)
          (shift constructor)
          (shift <$> patterns)
          (shift constructorInfo)
          Expression.lambdaVariable
          (Shift.map Shift.LetPattern thenx)
    }

replace ::
  Type scope ->
  Constructor2.Index scope ->
  ConstructorInfo scope ->
  Expression scope ->
  Statements (Scope.SimplePattern ':+ scope) ->
  Int ->
  Statements scope
replace _ _ _ _ thenx (-1) = Shift.map (Shift.Unshift fail) thenx
  where
    fail = error "bad irrefutable replace"
replace levity constructor constructorInfo check thenx n =
  LetOne
    { levity = Type.lazy,
      declaration =
        Expression.join_
          Bind
            { constructor,
              constructorInfo,
              check,
              thenx =
                Done
                  { done = Expression.patternVariableAt levity n
                  }
            },
      body =
        replace
          (shift levity)
          (shift constructor)
          (shift constructorInfo)
          (shift check)
          (Shift.map (Shift.ReplaceIrrefutable n) thenx)
          (n - 1)
    }

finish ::
  Bool ->
  Type scope ->
  Constructor.Index scope ->
  Strict.Vector (Pattern position Check scope) ->
  Semantic.ConstructorInfo scope ->
  Expression scope ->
  Statements (Scope.Pattern ':+ scope) ->
  Statements scope
finish irrefutable levity constructor patterns constructorInfo check thenx
  | all wildcard patterns,
    thenx <- Shift.map Shift.FinishPattern thenx =
      case constructorInfo of
        Semantic.Newtype ->
          LetOne
            { levity,
              declaration = Expression.newtype_ constructor check Expression.Destruct,
              body = Shift.map Shift.FinishNewtype thenx
            }
        Semantic.ConstructorInfo {entries}
          | irrefutable -> replace levity constructor constructorInfo check thenx (length patterns - 1)
          | otherwise -> Bind {constructor, constructorInfo, check, thenx}
          where
            constructorInfo = ConstructorInfo {entries}
  where
    wildcard Pattern.Wildcard {} = True
    wildcard _ = False
finish _ _ _ _ _ _ _ = error "bad finish"

true :: Pattern () Check scope
true =
  ( Pattern.Match
      { startPosition = (),
        levity = Solved Type.strict,
        names = Map.empty,
        match =
          Pattern.Constructor
            { constructorPosition = (),
              constructor = Constructor2.true,
              patterns = Strict.Vector.empty,
              constructorInfo = Solved Semantic.ConstructorInfo {entries = Strict.Vector.empty},
              failable = Unused
            },
        irrefutable = False
      }
  )

ifx :: Expression scope -> Expression scope -> Statements scope
ifx condition thenx = bind true condition Done {done = shift thenx}

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
      (Pattern.anonymize patternx)
      (Expression.simplify effect)
      (simplify thenx)
  Semantic.Let {declarations = Semantic.Declarations.Local declarations, body} ->
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
  LetOne {levity, declaration, body} ->
    LetOne
      { levity,
        declaration,
        body = call body (shift argument)
      }
  Branch {left, right} ->
    Branch
      { left = call left argument,
        right = call right argument
      }
  Bottom -> Bottom
