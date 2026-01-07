module Stage4.Index.Term where

import qualified Stage2.Index.Term as Stage2
import Stage2.Scope (Declaration, Environment (..), Global, Pattern)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Pattern :: !Int -> Index (Pattern ':+ scopes)
  Shift :: !(Index scopes) -> Index (scope ':+ scopes)
  Global :: !Int -> !Int -> Index Global

instance Show (Index scope) where
  showsPrec d = \case
    Declaration local -> showParen (d > 10) $ showString "Declaration " . showsPrec 11 local
    Pattern bound -> showParen (d > 10) $ showString "Pattern " . showsPrec 11 bound
    Shift index -> showParen (d > 10) $ showString "Shift " . showsPrec 11 index
    Global global local ->
      showParen (d > 10) $
        showString "Global "
          . showsPrec 11 global
          . showString " "
          . showsPrec 11 local

finish :: Stage2.Index scope -> Index scope
finish = \case
  Stage2.Declaration index -> Declaration index
  Stage2.Pattern patternx -> case patternx of
    Stage2.Select index Stage2.At -> Pattern index
    _ -> error "bad finish"
  Stage2.Shift index -> Shift (finish index)
  Stage2.Global global local -> Global global local
