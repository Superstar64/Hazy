module Stage2.Index.Term0 where

import qualified Stage2.Index.Term as Normal
import Stage2.Scope (Declaration, Environment (..), Global)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Global :: !Int -> !Int -> Index Global

instance Eq (Index scope) where
  Declaration local1 == Declaration local2 = local1 == local2
  Global global1 local1 == Global global2 local2 = global1 == global2 && local1 == local2

instance Show (Index scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Global global local ->
      showParen (d > 10) $
        showString "Global"
          . showsPrec 11 global
          . showString " "
          . showsPrec 11 local

normal :: Index scopes -> Normal.Index scopes
normal (Declaration index) = Normal.Declaration index
normal (Global global local) = Normal.Global global local
