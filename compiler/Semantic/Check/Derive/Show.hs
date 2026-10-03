module Semantic.Check.Derive.Show where

import {-# SOURCE #-} qualified Builtin.Show as Show
import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Bindings (..), Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import qualified Core.Tree.Class as Class
import Core.Tree.Data (Data)
import Core.Tree.EntryInfo (EntryInfo (..))
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.Statements (Statements (Bottom))
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (..))
import Semantic.Check.EntryInstance (entry)
import Semantic.Check.Simple.ConstructorInfo (ConstructorInfo (..))
import Semantic.Check.Simple.Data (instanciate)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..), cons)
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Constructor as Constructor (Syntax (..))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Lexer (runConstructorIdentifier, runConstructorSymbol, runVariableIdentifier, runVariableSymbol)
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity (..))
import qualified Syntax.Variable as Variable
import Prelude hiding (Eq)

printConstructor :: Bool -> Variable.Constructor -> Builder
printConstructor infixed = \case
  Variable.ConstructorIdentifier name -> open <> fromText (runConstructorIdentifier name) <> close
    where
      open =
        fromString $
          if infixed
            then
              "`"
            else ""
      close =
        fromString $
          if infixed
            then
              "`"
            else ""
  Variable.ConstructorSymbol name -> open <> fromText (runConstructorSymbol name) <> close
    where
      open =
        fromString $
          if infixed
            then
              ""
            else "("
      close =
        fromString $
          if infixed
            then
              ""
            else ")"

printField :: Variable.Variable -> Builder
printField = \case
  Variable.VariableIdentifier name -> fromText (runVariableIdentifier name)
  Variable.VariableSymbol name -> open <> fromText (runVariableSymbol name) <> close
    where
      open = fromString "("
      close = fromString ")"

showsPrec ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
showsPrec context position typeIndex datax = do
  DataInstance {types, constructors} <- instanciate context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] (toList types)
  let generate index constructor@ConstructorInstance {name, syntax, entries} = do
        let patterns :: Strict.Vector (Pattern scope)
            patterns = Strict.Vector.replicate (length entries) Wildcard
            types = entry <$> entries
        evidence <- traverse (Unify.constrain context position Type2.Show) types
        pure $ do
          evidence <- traverse (Unify.solveEvidence position) evidence
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
          let letter character =
                Expression.Lambda $
                  Expression.simplifyConstructorExact
                    Constructor.cons
                    ConstructorInfo
                      { entries =
                          Strict.Vector.fromList
                            [ EntryInfo {strict = Type.Constructor Type2.Lazy},
                              EntryInfo {strict = Type.Constructor Type2.Lazy}
                            ]
                      }
                    ( Strict.Vector.fromList
                        [ Expression.Character {character},
                          Expression.lambdaVariable
                        ]
                    )
              space = letter ' '
              identifier infixed =
                map letter $ unpack $ toStrict $ toLazyText $ printConstructor infixed name
              item index evidence =
                Expression.Method
                  { method = Method.showsPrec,
                    evidence = shift $ shift $ evidence,
                    instanciation = Instanciation.Mono,
                    methodInfo = Class.info $ Show.definition
                  }
                  `Expression.call` Expression.int (precedence + 1)
                  `Expression.call` Expression.patternVariableAt' index
              sections start end = case syntax of
                Constructor.Standard ->
                  concat
                    [ start,
                      identifier False,
                      concat $
                        map (\item -> [space, item]) $
                          zipWith item [0 ..] $
                            toList evidence,
                      end
                    ]
                Constructor.Infix {} ->
                  concat
                    [ start,
                      [item 0 (evidence Strict.Vector.! 0)],
                      [space],
                      identifier True,
                      [space],
                      [item 1 (evidence Strict.Vector.! 1)],
                      end
                    ]
                Constructor.Record fields ->
                  concat
                    [ start,
                      identifier False,
                      [space],
                      [letter '{'],
                      concat $
                        intersperse [letter ',', space] $
                          zipWith3 field [0 ..] (toList fields) (toList evidence),
                      [letter '}'],
                      end
                    ]
                  where
                    field index name evidence =
                      concat
                        [ map letter $ unpack $ toStrict $ toLazyText $ printField name,
                          [space],
                          [letter '='],
                          [space],
                          [item index evidence]
                        ]
              precedence = case syntax of
                Constructor.Standard -> 10
                Constructor.Infix Fixity {precedence} -> precedence
                Constructor.Record {} -> 10
              show start end =
                Expression.Lambda
                  { body =
                      let main = map shift (sections start end)
                       in foldr Expression.call Expression.lambdaVariable main
                  }

              branch
                | null entries = show [] []
                | otherwise =
                    Expression.ifx
                      ( Expression.lessThenEqualInt
                          (shift Expression.patternVariable)
                          (Expression.int precedence)
                      )
                      (show [] [])
                      (show [letter '('] [letter ')'])
              mainPattern =
                Match
                  { irrefutable = False,
                    match =
                      Constructor
                        { constructor =
                            Constructor.Index
                              { typeIndex = shift typeIndex,
                                constructorIndex = index
                              },
                          patterns,
                          constructorInfo = info
                        }
                  }
          pure
            Definition
              { definition =
                  Bound
                    { patternx = Pattern.Wildcard,
                      body =
                        Bound
                          { patternx = shift mainPattern,
                            body =
                              Plain
                                { plain = Done {done = branch}
                                }
                          }
                    }
              }
  definitions <- sequence $ zipWith generate [0 ..] (toList constructors)
  pure $ do
    definitions <- sequence definitions
    let bottom =
          Definition
            { definition = Plain {plain = Done {done = Expression.Join {statements = Bottom}}}
            }
        body = Definition.desugar $ foldr (<>) bottom definitions
    pure $ Generated $ Solved $ TypeLambda.mono body
