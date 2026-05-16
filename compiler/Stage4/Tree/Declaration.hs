module Stage4.Tree.Declaration where

import qualified Stage2.Index.Term as Stage2.Term
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import Stage2.Tree.Declaration (Key (..))
import qualified Stage3.Tree.Declaration as Stage3 (Declaration (..), LazyTermDeclaration (..))
import qualified Stage3.Tree.Definition2 as Stage3 (Choice (..), Definition2 (Definition, Piece))
import qualified Stage3.Tree.Definition2 as Stage3.Definition2
import qualified Stage3.Tree.Definition3 as Stage3 (Definition3 (..))
import qualified Stage3.Tree.Definition4 as Stage3 (Definition4 (..))
import qualified Stage3.Tree.Expression as Stage3 (Expression)
import qualified Stage3.Tree.Scheme as Stage3 (Scheme)
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import qualified Stage4.Temporary.Pattern as Pattern
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import Stage4.Tree.Scheme (Scheme)
import qualified Stage4.Tree.Scheme as Scheme
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver
import qualified Stage4.Tree.Statements as Statements

data LazyTermDeclaration scope
  = !Key :^ Declaration scope
  deriving (Show)

infix 4 :^

instance Shift LazyTermDeclaration where
  shift = shiftDefault

instance Shift.Functor LazyTermDeclaration where
  map = Shift2.mapDefault

instance Shift2.Functor LazyTermDeclaration where
  map = Substitute.mapDefault

instance Substitute.Functor LazyTermDeclaration where
  map category (name :^ declaration) = name :^ Substitute.map category declaration

data Declaration scope = Declaration
  { name :: !Key,
    definition :: !(SchemeOver Expression scope),
    typex :: !(Scheme scope)
  }
  deriving (Show)

instance Shift Declaration where
  shift = shiftDefault

instance Shift.Functor Declaration where
  map = Shift2.mapDefault

instance Shift2.Functor Declaration where
  map = Substitute.mapDefault

instance Substitute.Functor Declaration where
  map category Declaration {name, definition, typex} =
    Declaration
      { name,
        definition = Substitute.map category definition,
        typex = Substitute.map category typex
      }

simplify ::
  forall scope.
  Stage3.LazyTermDeclaration scope ->
  LazyTermDeclaration scope
simplify (name Stage3.:^ declaration) =
  name :^ case declaration of
    Stage3.Declaration
      { name,
        definition = _ Stage3.::: _ Stage3.::@ definition,
        typex
      } ->
        Declaration
          { name,
            definition = SchemeOver.map go definition,
            typex
          }
  where
    go :: SchemeOver.Map (Stage3.Definition2 source mark) Expression
    go = SchemeOver.Map $ \case
      Stage3.Definition definition _ -> Expression.simplify definition
      Stage3.Piece Stage3.Choice {index, instanciation, patternx, bound} _ ->
        Expression.Join
          { statements =
              Statements.bind
                (Pattern.simplify patternx)
                Expression.Variable
                  { variable = Term.from index,
                    instanciation
                  }
                Statements.Done
                  { done =
                      Expression.monoVariable $
                        Term.from $
                          Stage2.Term.Pattern bound
                  }
          }
      Stage3.Definition2.Shared shared _ -> Expression.simplify shared

annotation :: SchemeOver Stage3.Expression scope -> Stage3.Scheme position Check scope -> LazyTermDeclaration scope
annotation SchemeOver {parameters, constraints, result} scheme =
  Unnamed 0
    :^ Declaration
      { name = Unnamed 0,
        definition =
          SchemeOver
            { parameters,
              constraints,
              result = Expression.simplify result
            },
        typex = Scheme.simplify scheme
      }
