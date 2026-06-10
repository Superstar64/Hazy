module Semantic.Resolve.Temporary.Infix (Infix (..), Token (..), Pretoken (..), fixWith) where

import Data.Void (Void, absurd)
import Error (illegalFixity)
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Prelude hiding (Either (Left, Right), map)

data Infix x' x e
  = Infix e x (Infix x' x e)
  | Prefix x' (Infix x' x e)
  | Single e

data Stream x' x e
  = Literal e (Stream x' x e)
  | Token x (Stream x' x e)
  | Pretoken x' (Stream x' x e)
  | Done

stream :: Infix x' x e -> Stream x' x e
stream = \case
  Infix e x es -> Literal e (Token x (stream es))
  Prefix x' es -> Pretoken x' (stream es)
  Single e -> Literal e Done

data Result x' x e
  = Valid e (Stream x' x e)
  | Invalid Void

map :: (e -> e) -> Result x' x e -> Result x' x e
map f = \case
  Valid e es -> Valid (f e) es
  Invalid abort -> Invalid abort

class Token x where
  position :: x -> Position
  fixity :: x -> Fixity

class Pretoken x' where
  position' :: x' -> Position
  fixity' :: x' -> Int

instance Pretoken Void where
  position' = absurd
  fixity' = absurd

fixWith ::
  forall e x x'.
  (Token x, Pretoken x') =>
  (x' -> e -> e) ->
  (e -> x -> e -> e) ->
  Maybe Associativity ->
  Int ->
  Infix x' x e ->
  e
fixWith make' make associativity precedence infixed =
  case fixWith associativity precedence (stream infixed) of
    Valid e Done -> e
    Valid _ Literal {} -> error "double literal not reachable"
    Valid _ (Token x _) -> illegalFixity (position x)
    Valid _ (Pretoken x _) -> illegalFixity (position' x)
    Invalid abort -> absurd abort
  where
    match :: Maybe Associativity -> Associativity -> Bool
    match Nothing _ = True
    match (Just associativity) associativity' = associativity == associativity'

    fixWith :: Maybe Associativity -> Int -> Stream x' x e -> Result x' x e
    fixWith _ 10 stream = case stream of
      Literal e stream -> Valid e stream
      Token x _ -> Invalid $ illegalFixity $ position x
      Pretoken x _ -> Invalid $ illegalFixity $ position' x
      Done -> error "done not reachable"
    fixWith _ precedence (Pretoken x stream)
      | precedence == fixity' x = make' x `map` fixWith Nothing (precedence + 1) stream
    fixWith associativity precedence stream = case fixWith Nothing (precedence + 1) stream of
      Valid e (Token x stream)
        | Fixity {associativity = associativity', precedence = precedence'} <- fixity x,
          match associativity associativity',
          precedence == precedence' -> case associativity' of
            Right -> make e x `map` fixWith (Just Right) precedence stream
            None -> make e x `map` fixWith Nothing (precedence + 1) stream
            Left -> left e x stream
              where
                left e x stream = case fixWith Nothing (precedence + 1) stream of
                  Valid e2 (Token x2 stream)
                    | Fixity {associativity = Left, precedence = precedence'} <- fixity x2,
                      precedence == precedence' ->
                        left (make e x e2) x2 stream
                  result -> map (make e x) result
      result -> result
