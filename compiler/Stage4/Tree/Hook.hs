module Stage4.Tree.Hook where

import Data.Kind (Type)
import qualified Stage2.Index.Method as Method
import Stage2.Scope (Environment (..))
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Evidence (Evidence)

type Hook :: Environment -> Type
data Hook scope
  = DefaultNum
      { evidence :: !(Evidence scope),
        num :: !Method.Num
      }
  | DefaultEnum
      { evidence :: !(Evidence scope),
        enum :: !Method.Enum
      }
  | DefaultEq
      { evidence :: !(Evidence scope),
        eq :: !Method.Eq
      }
  | DefaultOrd
      { evidence :: !(Evidence scope),
        ord :: !Method.Ord
      }
  | DefaultReal
      { evidence :: !(Evidence scope),
        real :: !Method.Real
      }
  | DefaultIntegral
      { evidence :: !(Evidence scope),
        integral :: !Method.Integral
      }
  | DefaultFractional
      { evidence :: !(Evidence scope),
        fractional :: !Method.Fractional
      }
  | DefaultFunctor
      { evidence :: !(Evidence scope),
        functor :: !Method.Functor
      }
  | DefaultApplicative
      { evidence :: !(Evidence scope),
        applicative :: !Method.Applicative
      }
  | DefaultMonad
      { evidence :: !(Evidence scope),
        monad :: !Method.Monad
      }
  | DefaultMonadFail
      { evidence :: !(Evidence scope),
        monadFail :: !Method.MonadFail
      }
  deriving (Show)

instance Shift Hook where
  shift = shiftDefault

instance Shift.Functor Hook where
  map = Shift2.mapDefault

instance Shift2.Functor Hook where
  map = Substitute.mapDefault

instance Substitute.Functor Hook where
  map category = \case
    DefaultNum {evidence, num} ->
      DefaultNum
        { evidence = Substitute.map category evidence,
          num
        }
    DefaultEnum {evidence, enum} ->
      DefaultEnum
        { evidence = Substitute.map category evidence,
          enum
        }
    DefaultEq {evidence, eq} ->
      DefaultEq
        { evidence = Substitute.map category evidence,
          eq
        }
    DefaultOrd {evidence, ord} ->
      DefaultOrd
        { evidence = Substitute.map category evidence,
          ord
        }
    DefaultReal {evidence, real} ->
      DefaultReal
        { evidence = Substitute.map category evidence,
          real
        }
    DefaultIntegral {evidence, integral} ->
      DefaultIntegral
        { evidence = Substitute.map category evidence,
          integral
        }
    DefaultFractional {evidence, fractional} ->
      DefaultFractional
        { evidence = Substitute.map category evidence,
          fractional
        }
    DefaultFunctor {evidence, functor} ->
      DefaultFunctor
        { evidence = Substitute.map category evidence,
          functor
        }
    DefaultApplicative {evidence, applicative} ->
      DefaultApplicative
        { evidence = Substitute.map category evidence,
          applicative
        }
    DefaultMonad {evidence, monad} ->
      DefaultMonad
        { evidence = Substitute.map category evidence,
          monad
        }
    DefaultMonadFail {evidence, monadFail} ->
      DefaultMonadFail
        { evidence = Substitute.map category evidence,
          monadFail
        }
