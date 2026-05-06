module Stage3.Tree.Definition2 where

import Stage2.Index.Term (Bound)
import qualified Stage2.Scope as Scope (Show (..))
import Stage2.Tree.Definition2 (Share, Single)
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Pattern (Pattern)
import Stage3.Tree.RightHandSide (RightHandSide)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation)
import qualified Stage4.Tree.Type as Simple (Type)
import Prelude hiding (Maybe (Just))

data Definition2 source scope where
  Definition :: !(Definition scope) -> !(Simple.Type scope) -> Definition2 Single scope
  Piece :: !(Choice scope) -> !(Simple.Type scope) -> Definition2 Single scope
  Shared :: !(RightHandSide scope) -> !(Simple.Type scope) -> Definition2 Share scope

typex :: Definition2 source scope -> Simple.Type scope
typex = \case
  Definition _ typex -> typex
  Piece _ typex -> typex
  Shared _ typex -> typex

instance Show (Definition2 source scope) where
  showsPrec d = \case
    Definition definition typex ->
      showParen (d > 10) $
        showString "Definition "
          . showsPrec 11 definition
          . showString " "
          . showsPrec 11 typex
    Piece choice typex ->
      showParen (d > 10) $
        showString "Piece "
          . showsPrec 11 choice
          . showString " "
          . showsPrec 11 typex
    Shared shared typex ->
      showParen (d > 10) $
        showString "Shared "
          . showsPrec 11 shared
          . showString " "
          . showsPrec 11 typex

instance Scope.Show (Definition2 source) where
  showsPrec = showsPrec

data Choice scope = Choice
  { shareIndex :: !Int,
    instanciation :: !(Simple.Instanciation scope),
    patternx :: !(Pattern scope),
    bound :: !Bound
  }
  deriving (Show)
