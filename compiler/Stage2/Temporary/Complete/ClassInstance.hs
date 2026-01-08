module Stage2.Temporary.Complete.ClassInstance where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict.Vector
import Error (illegalInstanceData, instanceForNonClass)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1 (Declaration (..))
import Stage1.Tree.InstanceHead (InstanceHead (Head))
import qualified Stage1.Tree.InstanceHead as InstanceHead
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (ConstructorIdentifier, QualifiedConstructorIdentifier (..), Qualifiers (..))
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Context
  ( Context (..),
    (!=*~),
    (!=.*),
  )
import Stage2.Temporary.Complete.Method (Method (Method))
import qualified Stage2.Temporary.Complete.Method as Method
import Stage2.Temporary.Complete.TypeDeclaration (TypeDeclaration (TypeDeclaration))
import qualified Stage2.Temporary.Complete.TypeDeclaration as TypeDeclaration
import qualified Stage2.Tree.Instance as Real

data ClassInstance scope = ClassInstance
  { classPosition :: !Position,
    classIndex :: !Int,
    dataIndex :: !(Type2.Index scope),
    instancex :: Real.Instance scope
  }

shrink :: ClassInstance scope -> Real.Instance scope
shrink = instancex

prepare :: ClassInstance scope -> (Int, Map (Type2.Index scope) (NonEmpty.NonEmpty (ClassInstance scope)))
prepare instancex@ClassInstance {classIndex, dataIndex} =
  (classIndex, Map.singleton dataIndex (NonEmpty.singleton instancex))

resolve ::
  Context scope ->
  (ConstructorIdentifier -> Maybe (Int, TypeDeclaration scope)) ->
  Stage1.Declaration Position ->
  [ClassInstance scope]
resolve context lookup = \case
  Stage1.Instance
    { startPosition,
      prerequisites,
      className = Local :=. classx,
      instanceHead,
      instanceDefinition,
      classPosition
    }
      | Just (classIndex, TypeDeclaration {fields}) <- lookup classx ->
          let methods = case fields of
                TypeDeclaration.Methods methods -> methods
                _ -> instanceForNonClass classPosition
              memberMethods = Map.fromList $ zip [name | Method {name} <- toList methods] [0 ..]
              resolve dataIndex parameters =
                ClassInstance
                  { classIndex,
                    dataIndex,
                    classPosition,
                    instancex =
                      Real.resolve
                        context
                        startPosition
                        prerequisites
                        classPosition
                        parameters
                        memberMethods
                        instanceDefinition
                  }
                where

              entry = case instanceHead of
                Head {startPosition, typeName, parameters}
                  | dataIndex <- context !=.* startPosition :@ typeName ->
                      resolve (Type3.toType2 (illegalInstanceData startPosition) dataIndex) parameters
                InstanceHead.Lifted {startPosition, constructorName, parameters}
                  | index <- context !=*~ startPosition :@ constructorName ->
                      resolve (Type2.Lifted index) parameters
                InstanceHead.TupleN {count, parameters} ->
                  resolve (Type2.Tuple count) parameters
                InstanceHead.Tuple {parameters} ->
                  resolve (Type2.Tuple (length parameters)) parameters
                InstanceHead.List0 {} ->
                  resolve Type2.List Strict.Vector.empty
                InstanceHead.List {parameter} ->
                  resolve Type2.List (Strict.Vector.singleton parameter)
                InstanceHead.List1 {parameter} ->
                  resolve Type2.List (Strict.Vector.singleton parameter)
                InstanceHead.Arrow0 {} ->
                  resolve Type2.Arrow Strict.Vector.empty
                InstanceHead.Arrow1 {parameter} ->
                  resolve Type2.Arrow (Strict.Vector.singleton parameter)
                InstanceHead.Arrow2 {parameter, parameter'} ->
                  resolve Type2.Arrow (Strict.Vector.fromList [parameter, parameter'])
                InstanceHead.Arrow {parameter, parameter'} ->
                  resolve Type2.Arrow (Strict.Vector.fromList [parameter, parameter'])
           in [entry]
  _ -> []
