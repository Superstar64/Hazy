module Stage3.Unify.Error where

import Control.Monad (liftM2)
import Control.Monad.ST (ST)
import Data.List (nub)
import Data.STRef (STRef, readSTRef)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Error
  ( constraintError,
    escapingType,
    occurenceError,
    unificationError,
  )
import qualified Stage1.Lexer as Lexer
import Stage1.Position (Position)
import qualified Stage1.Printer as Stage1 (build)
import qualified Stage1.Tree.Type as Stage1 (Type (Call, argument, function, startPosition), print)
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Local as Table.Local
import qualified Stage2.Index.Table.Term as Table.Term
import qualified Stage2.Index.Table.Type as Table.Type
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Local as Label (LocalBinding (..))
import qualified Stage2.Label.Context as Label (Context (..))
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Shift as Shift
import qualified Stage2.Tree.Type as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Check.Context as Context
import Stage3.Unify.Class (Collected (..), Collector (..))
import qualified Stage3.Unify.Class as Collect
import Stage3.Unify.Type (Box (..), Type (..))

collect :: Type s scopes -> ST s [Collected s scopes]
collect = Collect.collect Collector

data Error s where
  Unify :: Context s scope -> Type s scope -> Type s scope -> Error s
  Occurs :: Context s scope -> STRef s (Box s scope) -> Type s scope -> Error s
  Constrain :: Context s scope -> Type2.Index scope -> Type s scope -> [Type s scope] -> Error s
  Unshift :: Context s (scope ':+ scopes) -> Type s (scope ':+ scopes) -> Error s

abort :: Position -> Error s -> ST s a
abort position = \case
  Unify context term1 term2 -> do
    unsolved <- nub <$> liftM2 (++) (collect term1) (collect term2)
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term1 <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term1
    term2 <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term2
    unificationError position term1 term2
  Occurs context reference term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
        infinite = case lookup (Collect reference) labeled of
          Nothing -> error "bad occurs lookup"
          Just variable ->
            let ast = Stage2.Variable {startPosition = (), variable}
             in Stage1.build $ Stage1.print $ Stage2.label temporary ast
    term <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term
    occurenceError position infinite term
  Constrain context classx constructor arguments -> do
    unsolved <- nub . concat <$> traverse collect (constructor : arguments)
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    fabricated <- fmap (Stage2.label temporary) <$> traverse (fabricate Shift.Shift labeled) arguments
    function <- Stage2.label temporary <$> fabricate Shift.Shift labeled (Constructor classx)
    argument <- Stage2.label temporary <$> fabricate Shift.Shift labeled constructor
    let head =
          Stage1.Call
            { startPosition = (),
              function,
              argument
            }
        call function argument =
          Stage1.Call
            { startPosition = (),
              function,
              argument
            }
        term = Stage1.build $ Stage1.print $ foldl call head fabricated
    constraintError position term
  Unshift context term -> do
    unsolved <- nub <$> collect term
    let labeled = zip unsolved [Local.Local i | i <- [0 ..]]
        temporary = temporaries (length unsolved) context
    term <- Stage1.build . Stage1.print . Stage2.label temporary <$> fabricate Shift.Shift labeled term
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
      ST s (Stage2.Type () scope')
    fabricate category names = \case
      Logical reference ->
        readSTRef reference >>= \case
          Solved typex -> fabricate category names typex
          Unsolved {} -> case lookup (Collect reference) names of
            Just variable ->
              pure
                Stage2.Variable
                  { startPosition = (),
                    variable
                  }
            Nothing -> error "uncollected variable"
      Shift typex -> fabricate (category Shift.:. Shift.Shift) names' typex
        where
          names' = [(collect, name) | (Reach collect, name) <- names]
      Variable variable ->
        pure $
          Stage2.Variable
            { startPosition = (),
              variable = Shift.map category variable
            }
      Constructor constructor ->
        pure $
          Shift.map category $
            Stage2.Constructor
              { startPosition = (),
                constructorPosition = (),
                constructor
              }
      Call function argument -> do
        function <- fabricate category names function
        argument <- fabricate category names argument
        pure $ Stage2.Call {function, argument}
      Function parameter result -> do
        parameter <- fabricate category names parameter
        result <- fabricate category names result
        pure $
          Stage2.Function
            { parameter,
              operatorPosition = (),
              result
            }
      Type universe -> do
        universe <- fabricate category names universe
        pure $
          Stage2.Type
            { startPosition = (),
              universe
            }
      Constraint -> pure $ Stage2.Constraint {startPosition = ()}
      Small -> pure $ Stage2.Small {startPosition = ()}
      Large -> pure $ Stage2.Large {startPosition = ()}
      Universe -> pure $ Stage2.Universe {startPosition = ()}
