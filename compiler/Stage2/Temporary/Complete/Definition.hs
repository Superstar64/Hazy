module Stage2.Temporary.Complete.Definition where

import Data.Foldable (toList)
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import qualified Stage1.Tree.LeftHandSide as Stage1 (LeftHandSide (..))
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (Variable))
import qualified Stage1.Tree.Pattern as Stage1.Pattern (Pattern (variable))
import qualified Stage1.Tree.PatternInfix as Stage1.Infix (startPosition)
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage1.Variable
  ( QualifiedVariable (..),
    Qualifiers (..),
    Variable,
  )
import Stage2.Layout (Normal)
import qualified Stage2.Resolve.Binding.Term as Term
import Stage2.Resolve.Context (Context, (!-))
import Stage2.Stage (Resolve)
import qualified Stage2.Temporary.PatternInfix as Pattern.Infix (fixWith, resolve)
import Stage2.Tree.Function (Function (..))
import qualified Stage2.Tree.Function as Function (resolve)
import qualified Stage2.Tree.Pattern as Pattern (augment)
import Prelude hiding (Either (Left, Right))

data Definition scope = Definition !Position !Variable (Function Normal Resolve scope)

resolve ::
  Definition scope ->
  Context scope ->
  Stage1.LeftHandSide Position ->
  Stage1.RightHandSide Position ->
  Definition scope
resolve failure context leftHandSide rightHandSide = case leftHandSide of
  Stage1.Pattern Stage1.Variable {variable = position :@ variable} ->
    Definition position variable $ Function.resolve context [] rightHandSide
  Stage1.Prefix {variable = position :@ variable, parameters'}
    | patterns <- toList parameters' ->
        Definition position variable $ Function.resolve context patterns rightHandSide
  Stage1.Binary
    { leftHandSide = patternx,
      operator = position :@ operator,
      rightHandSide = patternx',
      parameters = patterns
    }
      | functionPosition <- Stage1.Infix.startPosition patternx,
        functionPosition' <- Stage1.Infix.startPosition patternx',
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
