module Semantic.Resolve.Temporary.Complete.Definition where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
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
    } ->
      Definition position operator proper
      where
        functionPosition = Syntax.Infix.startPosition patternx
        functionPosition' = Syntax.Infix.startPosition patternx'
        Identity Term.Binding {fixity = Fixity {associativity, precedence}} =
          Term.value $ context !- position :@ Local :- operator
        proper
          | operators <- Pattern.Infix.resolve context patternx,
            patternx <- case associativity of
              Left -> Pattern.Infix.fixWith (Just Left) precedence operators
              _ -> Pattern.Infix.fixWith Nothing (precedence + 1) operators,
            context <- Pattern.augment patternx context,
            operators' <- Pattern.Infix.resolve context patternx',
            patternx' <- case associativity of
              Right -> Pattern.Infix.fixWith (Just Right) precedence operators'
              _ -> Pattern.Infix.fixWith Nothing (precedence + 1) operators',
            context <- Pattern.augment patternx' context =
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
