module Semantic.Resolve.Temporary.PatternInfix where

import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor (cons)
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import Semantic.Resolve.Context (Context (..), (!=~))
import qualified Semantic.Resolve.Go.Pattern as Pattern (resolve)
import Semantic.Resolve.Temporary.Infix (Infix (..))
import qualified Semantic.Resolve.Temporary.Infix as Infix
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Pattern (Pattern ())
import qualified Semantic.Tree.Pattern as Pattern (Pattern (..))
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import qualified Syntax.Tree.PatternInfix as Syntax (Infix (..))
import Prelude hiding (Either (Left, Right))

data Index scope
  = Constructor !Position !(Constructor.Binding scope)
  | Cons !Position

resolve :: Context scope -> Syntax.Infix Position -> Infix (Index scope) (Pattern Resolve scope)
resolve context = \case
  Syntax.Pattern {patternx} -> Single (Pattern.resolve context patternx)
  Syntax.Infix {left, operator, operatorPosition, right} ->
    Infix
      (Pattern.resolve context left)
      (Constructor operatorPosition $ context !=~ operator)
      (resolve context right)
  Syntax.InfixCons {left, operatorPosition, right} ->
    Infix (Pattern.resolve context left) (Cons operatorPosition) (resolve context right)

fixWith :: Maybe Associativity -> Int -> Infix (Index scope) (Pattern Resolve scope) -> Pattern Resolve scope
fixWith = Infix.fixWith position fixity operators
  where
    position = \case
      Constructor position _ -> position
      Cons position -> position
    fixity = \case
      Constructor _ Constructor.Binding {fixity} -> fixity
      Cons _ -> Fixity {associativity = Right, precedence = 5}
    operators pattern1 index pattern2 = case index of
      Constructor position Constructor.Binding {index, single} ->
        Pattern.Constructor
          { names = Map.empty,
            irrefutable = Prelude.False,
            constructorPosition = position,
            constructor = index,
            patterns = Strict.Vector.fromList [pattern1, pattern2],
            single,
            constructorInfo = Inferred
          }
      Cons constructorPosition ->
        Pattern.Constructor
          { names = Map.empty,
            irrefutable = Prelude.False,
            constructorPosition,
            constructor = Constructor.cons,
            patterns = Strict.Vector.fromList [pattern1, pattern2],
            single = False,
            constructorInfo = Inferred
          }

fix :: Infix (Index scope) (Pattern Resolve scope) -> Pattern Resolve scope
fix = fixWith Nothing 0
