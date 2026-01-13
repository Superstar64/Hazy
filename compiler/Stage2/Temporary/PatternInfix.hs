module Stage2.Temporary.PatternInfix where

import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import qualified Stage1.Tree.PatternInfix as Stage1 (Infix (..))
import qualified Stage2.Index.Constructor as Constructor (cons)
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import Stage2.Resolve.Context
  ( Context (..),
    (!=~),
  )
import Stage2.Temporary.Infix (Infix (..))
import qualified Stage2.Temporary.Infix as Infix
import Stage2.Tree.Pattern (Match (..), Pattern (..))
import qualified Stage2.Tree.Pattern as Pattern
import Prelude hiding (Either (Left, Right))

data Index scope
  = Constructor !Position !(Constructor.Binding scope)
  | Cons !Position

resolve :: Context scope -> Stage1.Infix Position -> Infix (Index scope) (Pattern scope)
resolve context = \case
  Stage1.Pattern {patternx} -> Single (Pattern.resolve context patternx)
  Stage1.Infix {left, operator, operatorPosition, right} ->
    Infix
      (Pattern.resolve context left)
      (Constructor operatorPosition $ context !=~ operator)
      (resolve context right)
  Stage1.InfixCons {left, operatorPosition, right} ->
    Infix (Pattern.resolve context left) (Cons operatorPosition) (resolve context right)

fixWith :: Maybe Associativity -> Int -> Infix (Index scope) (Pattern scope) -> Pattern scope
fixWith = Infix.fixWith position fixity operators
  where
    position = \case
      Constructor position _ -> position
      Cons position -> position
    fixity = \case
      Constructor _ Constructor.Binding {fixity} -> fixity
      Cons _ -> Fixity {associativity = Right, precedence = 5}
    operators pattern1 index pattern2 = case index of
      Constructor position Constructor.Binding {index} ->
        At
          { names = Map.empty,
            match =
              Match $
                Pattern.Constructor
                  { constructorPosition = position,
                    constructor = index,
                    patterns = Strict.Vector.fromList [pattern1, pattern2]
                  }
          }
      Cons constructorPosition ->
        At
          { names = Map.empty,
            match =
              Match $
                Pattern.Constructor
                  { constructorPosition,
                    constructor = Constructor.cons,
                    patterns = Strict.Vector.fromList [pattern1, pattern2]
                  }
          }

fix :: Infix (Index scope) (Pattern scope) -> Pattern scope
fix = fixWith Nothing 0
