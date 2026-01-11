module Stage2.Temporary.Infix where

import Control.Arrow (first)
import Error (illegalFixity)
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Prelude hiding (Either (Left, Right))

data Infix x e
  = Infix e x (Infix x e)
  | Single e

data Infix' x e
  = Oped x (Infix x e)
  | Empty

fixWith ::
  forall e x.
  (x -> Position) ->
  (x -> Fixity) ->
  (e -> x -> e -> e) ->
  Maybe Associativity ->
  Int ->
  Infix x e ->
  e
fixWith position fixity make = \target precedence operators -> case fixWith target precedence operators of
  (e, Empty) -> e
  (_, Oped x _) -> illegalFixity (position x)
  where
    fixWith :: Maybe Associativity -> Int -> Infix x e -> (e, Infix' x e)
    fixWith _ 10 operators = case operators of
      Infix e x operators -> (e, Oped x operators)
      Single e -> (e, Empty)
    fixWith target precedence operators = case fixWith Nothing (precedence + 1) operators of
      (e, Oped index operators)
        | Fixity {associativity, precedence = precedence'} <- fixity index,
          case target of Nothing -> True; Just associativity' -> associativity == associativity',
          precedence == precedence' -> case associativity of
            Right -> first (make e index) (fixWith (Just Right) precedence operators)
            None -> first (make e index) (fixWith Nothing (precedence + 1) operators)
            Left -> left e operators
              where
                left e operators = case fixWith Nothing (precedence + 1) operators of
                  (e2, Oped index operators)
                    | Fixity {associativity = associativity', precedence = precedence'} <-
                        fixity index,
                      precedence == precedence',
                      associativity == associativity' ->
                        left (make e index e2) operators
                  (e2, oped) -> (make e index e2, oped)
      es -> es
