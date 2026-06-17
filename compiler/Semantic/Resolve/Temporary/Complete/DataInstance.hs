module Semantic.Resolve.Temporary.Complete.DataInstance where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Error (illegalInstanceClass)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Binding.Type as Type
import Semantic.Resolve.Context (Context, (!=.))
import qualified Semantic.Resolve.Go.Instance as Real (resolve)
import qualified Semantic.Stage as Semantic
import qualified Semantic.Tree.Instance as Real (Instance)
import Syntax.Position (Position)
import qualified Syntax.Tree.Declaration as Syntax
import Syntax.Tree.InstanceHead (InstanceHead (Head))
import qualified Syntax.Tree.InstanceHead as InstanceHead
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (ConstructorIdentifier, QualifiedConstructorIdentifier ((:=.)), Qualifiers (Local))

data DataInstance scope = DataInstance
  { classPosition :: !Position,
    dataIndex :: !Int,
    classIndex :: !(Type2.Index scope),
    instancex :: Real.Instance Normal Semantic.Resolve scope
  }

shrink :: DataInstance scope -> Real.Instance Normal Semantic.Resolve scope
shrink = instancex

prepare :: DataInstance scope -> (Int, Map (Type2.Index scope) (NonEmpty.NonEmpty (DataInstance scope)))
prepare instancex@DataInstance {dataIndex, classIndex} =
  (dataIndex, Map.singleton classIndex (NonEmpty.singleton instancex))

newtype Resolve = Resolve
  { typeIndexes :: Map ConstructorIdentifier Int
  }

resolve ::
  Context scope ->
  (ConstructorIdentifier -> Maybe Int) ->
  Syntax.Declaration Position ->
  [DataInstance scope]
resolve context lookup = \case
  Syntax.Instance
    { startPosition,
      prerequisites,
      classPosition,
      className,
      instanceHead = Head {typeName = Local :=. datax, parameters},
      instanceDefinition
    }
      | Just dataIndex <- lookup datax,
        valid ->
          let Type.Binding {index, methods} = context !=. classPosition :@ className
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
                        parameters
                        methods
                        instanceDefinition
                  }
           in [entry]
      where
        valid
          | Local :=. classx <- className,
            Just _ <- lookup classx =
              False
          | otherwise = True
  _ -> []
