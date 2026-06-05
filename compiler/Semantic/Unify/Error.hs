module Semantic.Unify.Error where

import Control.Monad (liftM2)
import Control.Monad.ST (ST)
import Data.List (nub)
import Data.STRef (STRef, readSTRef)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Error
  ( constraintError,
    escapingType,
    maskError,
    occurenceError,
    unificationError,
  )
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Context as Context
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Local as Table.Local
import qualified Semantic.Index.Table.Term as Table.Term
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Local as Label (LocalBinding (..))
import qualified Semantic.Label.Context as Label (Context (..))
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Stage (Equal (..), Resolve)
import Semantic.Tree.Type (Synonym (NoSynonym))
import qualified Semantic.Tree.Type as Semantic
import Semantic.Unify.Class (Collected (..), Collector (..))
import qualified Semantic.Unify.Class as Collect
import Semantic.Unify.Type (Box (..), Type (..))
import qualified Syntax.Lexer as Lexer
import Syntax.Position (Position)
import qualified Syntax.Printer as Syntax (build)
import qualified Syntax.Tree.Type as Syntax (Type (Call, argument, function, startPosition), print)

collect :: Type s scopes -> ST s [Collected s scopes]
collect = Collect.collect (Collector Mask.Inline)

data Error s where
  Unify :: Context s scope -> Type s scope -> Type s scope -> Error s
  Occurs :: Context s scope -> STRef s (Box s scope) -> Type s scope -> Error s
  Mismask :: Context s scope -> Type s scope -> Error s
  Constrain :: Context s scope -> Type2.Index scope -> Type s scope -> [Type s scope] -> Error s
  Unshift :: Context s (scope ':+ scopes) -> Type s (scope ':+ scopes) -> Error s

abort :: Position -> Error s -> ST s a
abort position = \case
  Unify context term1 term2 -> do
    unsolved <- nub <$> liftM2 (++) (collect term1) (collect term2)
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term1 <- Syntax.build . Syntax.print . Semantic.label temporary <$> fabricate Shift.Shift labeled term1
    term2 <- Syntax.build . Syntax.print . Semantic.label temporary <$> fabricate Shift.Shift labeled term2
    unificationError position term1 term2
  Occurs context reference term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
        infinite = case lookup (Collect reference) labeled of
          Nothing -> error "bad occurs lookup"
          Just variable ->
            let ast = Semantic.Variable {startPosition = (), variable}
             in Syntax.build $ Syntax.print $ Semantic.label temporary ast
    term <- Syntax.build . Syntax.print . Semantic.label temporary <$> fabricate Shift.Shift labeled term
    occurenceError position infinite term
  Mismask context term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term <- Syntax.build . Syntax.print . Semantic.label temporary <$> fabricate Shift.Shift labeled term
    maskError position term
  Constrain context classx constructor arguments -> do
    unsolved <- nub . concat <$> traverse collect (constructor : arguments)
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    fabricated <- fmap (Semantic.label temporary) <$> traverse (fabricate Shift.Shift labeled) arguments
    function <- Semantic.label temporary <$> fabricate Shift.Shift labeled (Constructor classx)
    argument <- Semantic.label temporary <$> fabricate Shift.Shift labeled constructor
    let head =
          Syntax.Call
            { startPosition = (),
              function,
              argument
            }
        call function argument =
          Syntax.Call
            { startPosition = (),
              function,
              argument
            }
        term = Syntax.build $ Syntax.print $ foldl call head fabricated
    constraintError position term
  Unshift context term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term <- Syntax.build . Syntax.print . Semantic.label temporary <$> fabricate Shift.Shift labeled term
    escapingType position term
  where
    temporaries :: Int -> Context s scopes -> Label.Context (Scope.Local ':+ scopes)
    temporaries length context = case Context.label context of
      Label.Context {terms, locals, types} ->
        Label.Context
          { terms = Table.Term.Local terms,
            locals = Table.Local.Local names locals,
            types = Table.Type.Local types
          }
        where
          names = Vector.fromList $ do
            i <- [0 .. length]
            let name = Lexer.variableIdentifier (Text.pack $ "__flexible_" ++ show i)
            pure Label.LocalBinding {name}
    fabricate ::
      Shift.Category scope scope' ->
      [(Collected s scope, Local.Index scope')] ->
      Type s scope ->
      ST s (Semantic.Type () Resolve scope')
    fabricate category names = \case
      Logical reference ->
        readSTRef reference >>= \case
          Solved typex -> fabricate category names typex
          Unsolved {} -> case lookup (Collect reference) names of
            Just variable ->
              pure
                Semantic.Variable
                  { startPosition = (),
                    variable
                  }
            Nothing -> error "uncollected variable"
      Shift typex -> fabricate (category Shift.:. Shift.Shift) names' typex
        where
          names' = [(collect, name) | (Reach collect, name) <- names]
      Variable variable ->
        pure $
          Semantic.Variable
            { startPosition = (),
              variable = Shift.map category variable
            }
      Constructor constructor ->
        pure $
          Shift.map category $
            Semantic.Constructor
              { startPosition = (),
                constructorPosition = (),
                constructor,
                synonym = NoSynonym
              }
      Call function argument -> do
        function <- fabricate category names function
        argument <- fabricate category names argument
        pure $
          Semantic.Call
            { startPosition = (),
              function,
              argument
            }
      Function parameter result -> do
        parameter <- fabricate category names parameter
        result <- fabricate category names result
        pure $
          Semantic.Function
            { startPosition = (),
              parameter,
              operatorPosition = (),
              result
            }
      Type universe -> do
        universe <- fabricate category names universe
        pure $
          Semantic.Type
            { startPosition = (),
              universe,
              unsupported = Refl
            }
      Constraint -> pure $ Semantic.Constraint {startPosition = ()}
      Small -> pure $ Semantic.Small {startPosition = (), unsupported = Refl}
      Large -> pure $ Semantic.Large {startPosition = (), unsupported = Refl}
      Universe -> pure $ Semantic.Universe {startPosition = (), unsupported = Refl}
      Levity -> pure $ Semantic.Levity {startPosition = ()}
