module Stage2.Temporary.ExpressionInfix where

import Data.List.Reverse (List (..))
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import qualified Stage1.Tree.ExpressionInfix as Stage1 (Infix (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable
  ( QualifiedName (..),
  )
import qualified Stage2.Index.Constructor as Constructor (cons)
import Stage2.Layout (Normal)
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Resolve.Binding.Term as Term
import Stage2.Resolve.Context
  ( Context (..),
    (!-),
    (!=~),
  )
import Stage2.Stage (Resolve)
import Stage2.Temporary.Infix (Infix (..))
import qualified Stage2.Temporary.Infix as Infix
import Stage2.Tree.Expression (Expression)
import qualified Stage2.Tree.Expression as Expression
  ( resolve,
    resolveConstructor2,
    resolveTerm2,
  )
import Prelude hiding (Either (Left, Right))

data Index scope
  = Term !Position !(Term.Binding scope)
  | Constructor !Position !(Constructor.Binding scope)
  | Cons !Position

resolve :: Context scope -> Stage1.Infix Position -> Infix (Index scope) (Expression Normal Resolve scope)
resolve context = \case
  Stage1.Expression expression1 -> Single (Expression.resolve context expression1)
  Stage1.Infix {left, operator, right} ->
    Infix (Expression.resolve context left) (lookup operator) (resolve context right)
    where
      lookup = \case
        operatorPosition :@ QualifiedVariable operator ->
          Term operatorPosition (context !- (operatorPosition :@ operator))
        operatorPosition :@ QualifiedConstructor operator ->
          Constructor operatorPosition (context !=~ operatorPosition :@ operator)
  Stage1.InfixCons {head, operatorPosition, tail} ->
    Infix (Expression.resolve context head) (Cons operatorPosition) (resolve context tail)

fix :: Infix (Index scope) (Expression Normal Resolve scope) -> Expression Normal Resolve scope
fix = fixWith Nothing 0

fixWith ::
  Maybe Associativity ->
  Int ->
  Infix (Index scope) (Expression Normal Resolve scope) ->
  Expression Normal Resolve scope
fixWith = Infix.fixWith position fixity operator
  where
    position = \case
      Term position _ -> position
      Constructor position _ -> position
      Cons position -> position
    fixity = \case
      Term _ Term.Binding {fixity} -> fixity
      Constructor _ Constructor.Binding {fixity} -> fixity
      Cons _ -> Fixity {associativity = Right, precedence = 5}
    operator ::
      Expression Normal Resolve scope ->
      Index scope ->
      Expression Normal Resolve scope ->
      Expression Normal Resolve scope
    operator left index right = case index of
      Term position Term.Binding {index} ->
        Expression.resolveTerm2 position index (Nil :> left :> right)
      Constructor position Constructor.Binding {index} ->
        Expression.resolveConstructor2 position index (Nil :> left :> right)
      Cons position ->
        Expression.resolveConstructor2 position Constructor.cons (Nil :> left :> right)
