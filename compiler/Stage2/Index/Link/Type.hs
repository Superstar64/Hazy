module Stage2.Index.Link.Type where

import qualified Stage2.Index.Type0 as Type0
import Stage2.Locality (Global, Local)
import qualified Stage2.Scope as Scope

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

global :: Type0.Index Scope.Global -> Link Global
global (Type0.Global global local) = Global global local

local :: Type0.Index (Scope.Declaration 'Scope.:+ scope) -> Link Local
local (Type0.Declaration index) = Declaration index

unglobal :: Link Global -> Type0.Index Scope.Global
unglobal (Global global local) = Type0.Global global local

unlocal :: Link Local -> Type0.Index (Scope.Declaration 'Scope.:+ scope)
unlocal (Declaration index) = Type0.Declaration index
