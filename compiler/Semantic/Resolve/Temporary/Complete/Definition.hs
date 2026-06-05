module Semantic.Resolve.Temporary.Complete.Definition where

import Data.Foldable (toList)
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Binding.Term as Term
import Semantic.Resolve.Context (Context, (!-))
import qualified Semantic.Resolve.Go.Function as Function (resolve)
import qualified Semantic.Resolve.Go.Pattern as Pattern (augment)
import qualified Semantic.Resolve.Temporary.PatternInfix as Pattern.Infix (fixWith, resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Function (Function (..))
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import qualified Syntax.Tree.LeftHandSide as Syntax (LeftHandSide (..))
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Pattern as Syntax (Pattern (Variable))
import qualified Syntax.Tree.Pattern as Syntax.Pattern (Pattern (variable))
import qualified Syntax.Tree.PatternInfix as Syntax.Infix (startPosition)
import qualified Syntax.Tree.RightHandSide as Syntax (RightHandSide (..))
import Syntax.Variable
  ( QualifiedVariable (..),
    Qualifiers (..),
    Variable,
  )
import Prelude hiding (Either (Left, Right))

data Definition scope = Definition !Position !Variable (Function Normal Resolve scope)

resolve ::
  Definition scope ->
  Context scope ->
  Syntax.LeftHandSide Position ->
  Syntax.RightHandSide Position ->
  Definition scope
resolve failure context leftHandSide rightHandSide = case leftHandSide of
  Syntax.Pattern Syntax.Variable {variable = position :@ variable} ->
    Definition position variable $ Function.resolve context [] rightHandSide
  Syntax.Prefix {variable = position :@ variable, parameters'}
    | patterns <- toList parameters' ->
        Definition position variable $ Function.resolve context patterns rightHandSide
  Syntax.Binary
    { leftHandSide = patternx,
      operator = position :@ operator,
      rightHandSide = patternx',
      parameters = patterns
    }
      | functionPosition <- Syntax.Infix.startPosition patternx,
        functionPosition' <- Syntax.Infix.startPosition patternx',
        ~Term.Binding {fixity = Fixity {associativity, precedence}} <-
          context !- (position :@ Local :- operator),
        patternx <- case associativity of
          Left -> Pattern.Infix.fixWith (Just Left) precedence $ Pattern.Infix.resolve context patternx
          _ -> Pattern.Infix.fixWith Nothing (precedence + 1) $ Pattern.Infix.resolve context patternx,
        context <- Pattern.augment patternx context,
        patternx' <- case associativity of
          Right -> Pattern.Infix.fixWith (Just Right) precedence $ Pattern.Infix.resolve context patternx'
          _ -> Pattern.Infix.fixWith Nothing (precedence + 1) $ Pattern.Infix.resolve context patternx',
        context <- Pattern.augment patternx' context ->
          Definition position operator $
            Bound
              { functionPosition,
                patternx,
                function =
                  Bound
                    { functionPosition = functionPosition',
                      patternx = patternx',
                      function = Function.resolve context (toList patterns) rightHandSide
                    }
              }
  _ -> failure
