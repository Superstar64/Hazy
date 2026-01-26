module Stage4.Tree.Evidence (Evidence (..)) where

import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import qualified Stage4.Shift as Shift2
import Stage4.Substitute (Category (Substitute))
import qualified Stage4.Substitute as Substitute

data Evidence scope
  = Variable
      { variable :: !(Evidence.Index scope)
      }
  | Call
      { function :: !(Evidence scope),
        arguments :: !(Strict.Vector (Evidence scope))
      }
  | Super
      { base :: !(Evidence scope),
        index :: !Int
      }
  deriving (Show)

instance Shift Evidence where
  shift = shiftDefault

instance Shift.Functor Evidence where
  map = Shift2.mapDefault

instance Shift2.Functor Evidence where
  map = Substitute.mapDefault

instance Substitute.Functor Evidence where
  map (Substitute.Lift category) Variable {variable} =
    Variable
      { variable = Shift2.map category variable
      }
  map Substitute.Over {} Variable {variable = Evidence.Index (Evidence0.Assumed index)} =
    Variable
      { variable = Evidence.Index $ Evidence0.Assumed index
      }
  map (Substitute.Over category) Variable {variable = Evidence.Index (Evidence0.Shift index)} =
    shift $
      Substitute.map category Variable {variable = Evidence.Index index}
  map (Substitute _ _ replacements) Variable {variable = Evidence.Index (Evidence0.Assumed index)} =
    replacements Vector.! index
  map (Substitute category _ _) Variable {variable = Evidence.Index (Evidence0.Shift index)} =
    Variable {variable = Shift.map category $ Evidence.Index index}
  map category evidence = case evidence of
    Variable {variable} ->
      Variable
        { variable =
            let map1 :: Category scope1 scope2 -> Type.Index scope1 -> Type.Index scope2
                map1 (Substitute.Lift category) index = Shift2.map category index
                map1 (Substitute category _ _) index = Shift.map category $ Type.unlocal index
                map1 (Substitute.Over category) (Type.Shift index) = Type.Shift $ map1 category index
                map1 Substitute.Over {} (Type.Declaration index) = Type.Declaration index
                map2 = Type2.map . map1
             in case variable of
                  Evidence.Builtin builtin -> Evidence.Builtin builtin
                  Evidence.Class index1 index2 -> Evidence.Class (map1 category index1) (map2 category index2)
                  Evidence.Data index1 index2 -> Evidence.Data (map2 category index1) (map1 category index2)
        }
    Call {function, arguments} ->
      Call
        { function = Substitute.map category function,
          arguments = Substitute.map category <$> arguments
        }
    Super {base, index} ->
      Super
        { base = Substitute.map category base,
          index
        }
