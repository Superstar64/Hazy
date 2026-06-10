module Semantic.Resolve.Temporary.ExpressionInfix where

import Data.List.Reverse (List (..))
import qualified Semantic.Index.Constructor as Constructor (cons)
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Semantic.Resolve.Binding.Term as Term
import Semantic.Resolve.Context
  ( Context (..),
    (!-),
    (!=~),
  )
import qualified Semantic.Resolve.Go.Expression as Expression
  ( resolve,
    resolveConstructor2,
    resolveTerm2,
  )
import Semantic.Resolve.Temporary.Infix (Infix (..))
import qualified Semantic.Resolve.Temporary.Infix as Infix
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (Inferred))
import Semantic.Tree.Expression (Expression)
import qualified Semantic.Tree.Expression as Expression (Expression (..))
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import qualified Syntax.Tree.ExpressionInfix as Syntax (Infix (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (QualifiedName (..))
import Prelude hiding (Either (Left, Right))

data Index scope
  = Term !Position !(Term.Binding scope)
  | Constructor !Position !(Constructor.Binding scope)
  | Cons !Position

instance Infix.Token (Index scope) where
  position = \case
    Term position _ -> position
    Constructor position _ -> position
    Cons position -> position
  fixity = \case
    Term _ Term.Binding {fixity} -> fixity
    Constructor _ Constructor.Binding {fixity} -> fixity
    Cons _ -> Fixity {associativity = Right, precedence = 5}

newtype Negate = Negate Position

instance Infix.Pretoken Negate where
  position' (Negate position) = position
  fixity' _ = 6

resolve :: Context scope -> Syntax.Infix Position -> Infix Negate (Index scope) (Expression Normal Resolve scope)
resolve context = \case
  Syntax.Expression expression1 -> Single (Expression.resolve context expression1)
  Syntax.Infix {left, operator, right} ->
    Infix (Expression.resolve context left) (lookup operator) (resolve context right)
    where
      lookup = \case
        operatorPosition :@ QualifiedVariable operator ->
          Term operatorPosition (context !- (operatorPosition :@ operator))
        operatorPosition :@ QualifiedConstructor operator ->
          Constructor operatorPosition (context !=~ operatorPosition :@ operator)
  Syntax.InfixCons {head, operatorPosition, tail} ->
    Infix (Expression.resolve context head) (Cons operatorPosition) (resolve context tail)
  Syntax.Negate {startPosition, negative} -> Prefix (Negate startPosition) (resolve context negative)

fix :: Infix Negate (Index scope) (Expression Normal Resolve scope) -> Expression Normal Resolve scope
fix = fixWith Nothing 0

fixWith ::
  Maybe Associativity ->
  Int ->
  Infix Negate (Index scope) (Expression Normal Resolve scope) ->
  Expression Normal Resolve scope
fixWith = Infix.fixWith negate operator
  where
    negate ::
      Negate ->
      Expression Normal Resolve scope ->
      Expression Normal Resolve scope
    negate (Negate startPosition) negative =
      Expression.Negate {startPosition, evidence = Inferred, negative}

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
