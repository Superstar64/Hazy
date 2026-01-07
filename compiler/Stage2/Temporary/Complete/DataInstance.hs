module Stage2.Temporary.Complete.DataInstance where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Error (illegalInstanceClass)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1
import Stage1.Tree.InstanceHead (InstanceHead (Head))
import qualified Stage1.Tree.InstanceHead as InstanceHead
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (ConstructorIdentifier, QualifiedConstructorIdentifier ((:=.)), Qualifiers (Local))
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Binding.Type as Type
import Stage2.Resolve.Context (Context, (!=.))
import qualified Stage2.Tree.Instance as Real

data DataInstance scope = DataInstance
  { classPosition :: !Position,
    dataIndex :: !Int,
    classIndex :: !(Type2.Index scope),
    instancex :: Real.Instance scope
  }

shrink :: DataInstance scope -> Real.Instance scope
shrink = instancex

prepare :: DataInstance scope -> (Int, Map (Type2.Index scope) (NonEmpty.NonEmpty (DataInstance scope)))
prepare instancex@DataInstance {dataIndex, classIndex} =
  (dataIndex, Map.singleton classIndex (NonEmpty.singleton instancex))

newtype Resolve = Resolve
  { typeIndexes :: Map ConstructorIdentifier Int
  }

resolve :: Context scope -> Resolve -> Stage1.Declaration Position -> [DataInstance scope]
resolve context Resolve {typeIndexes} = \case
  Stage1.Instance
    { Stage1.startPosition,
      Stage1.prerequisites,
      Stage1.classPosition,
      Stage1.className,
      Stage1.instanceHead = Head {InstanceHead.typeName = Local :=. datax, InstanceHead.parameters},
      Stage1.instanceDefinition
    }
      | Just dataIndex <- Map.lookup datax typeIndexes,
        valid ->
          let Type.Binding {Type.index, Type.methods} = context !=. classPosition :@ className
              classIndex = Type3.toType2 (illegalInstanceClass classPosition) index
              entry =
                DataInstance
                  { classPosition,
                    dataIndex,
                    classIndex,
                    instancex =
                      Real.resolve
                        context
                        startPosition
                        prerequisites
                        classPosition
                        parameters
                        methods
                        instanceDefinition
                  }
           in [entry]
      where
        valid
          | Local :=. classx <- className,
            Map.member classx typeIndexes =
              False
          | otherwise = True
  _ -> []
