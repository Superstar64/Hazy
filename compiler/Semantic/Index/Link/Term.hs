module Semantic.Index.Link.Term where

import qualified Semantic.Index.Term0 as Term0
import Semantic.Locality (Global, Local)
import qualified Semantic.Scope as Scope

data Link locality where
  Declaration :: !Int -> Link Local
  Global :: !Int -> !Int -> Link Global

instance Eq (Link scope) where
  Declaration local1 == Declaration local2 = local1 == local2
  Global global1 local1 == Global global2 local2 = global1 == global2 && local1 == local2

instance Ord (Link scope) where
  Declaration index1 `compare` Declaration index2 = index1 `compare` index2
  Global global1 local1 `compare` Global global2 local2 =
    compare (global1, local1) (global2, local2)

instance Show (Link scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Global global local ->
      showParen (d > 10) $
        showString "Global "
          . showsPrec 11 global
          . showString " "
          . showsPrec 11 local

global :: Term0.Index Scope.Global -> Link Global
global (Term0.Global global local) = Global global local

local :: Term0.Index (Scope.Declaration 'Scope.:+ scope) -> Link Local
local (Term0.Declaration index) = Declaration index

unglobal :: Link Global -> Term0.Index Scope.Global
unglobal (Global global local) = Term0.Global global local

unlocal :: Link Local -> Term0.Index (Scope.Declaration 'Scope.:+ scope)
unlocal (Declaration index) = Term0.Declaration index
