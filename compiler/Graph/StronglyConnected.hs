-- |
-- This module implements Tarjan's strongly connected components algorithm. This
-- is a variant that lazily generates strongly connected components on demand.
-- See https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm#The_algorithm_in_pseudocode
module Graph.StronglyConnected
  ( Component (..),
    Index (..),
    tarjan,
  )
where

import Control.Applicative (liftA)
import Control.Monad (when)
import Control.Monad.ST (ST, stToIO)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..), sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.STRef
  ( STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

data Component k
  = Group {set :: !(Set k)}
  | Link
      { link :: !k,
        id :: !Int
      }
  deriving (Eq, Show)

data State s k = State
  { index :: !(STRef s Int),
    lowlink :: !(STRef s Int),
    onStack :: !(STRef s Bool),
    value :: k,
    result :: !(STRef s (Component k))
  }

initialize :: k -> ST s (State s k)
initialize value = do
  index <- newSTRef $! negate 1
  lowlink <- newSTRef $! negate 1
  onStack <- newSTRef False
  result <- newSTRef undefined
  pure State {index, lowlink, onStack, value, result}

visiter :: (Ord k) => (k -> [State s k]) -> ST s (State s k -> ST s ())
visiter children = do
  indexRef <- newSTRef 0
  stackRef <- newSTRef []
  let strongconnect v = do
        indexValue <- readSTRef indexRef
        writeSTRef (index v) indexValue
        writeSTRef (lowlink v) indexValue
        writeSTRef indexRef $! (indexValue + 1)
        modifySTRef' stackRef (v :)
        writeSTRef (onStack v) True
        for_ (children $ value v) $ \w -> do
          indexw <- readSTRef (index w)
          onstackw <- readSTRef (onStack w)
          if
            | indexw == negate 1 -> do
                strongconnect w
                lowlinkv <- readSTRef (lowlink v)
                lowlinkw <- readSTRef (lowlink w)
                writeSTRef (lowlink v) $! min lowlinkv lowlinkw
            | onstackw -> do
                lowlinkv <- readSTRef (lowlink v)
                writeSTRef (lowlink v) $! min lowlinkv indexw
            | otherwise -> pure ()
        lowlinkv <- readSTRef (lowlink v)
        indexv <- readSTRef (index v)
        when (lowlinkv == indexv) $ do
          nodes <- collect
          let root :| children = sortOn value nodes
              set = Set.fromAscList (value root : map value children)
          writeSTRef (result root) Group {set}
          for_ (zip [0 ..] children) $ \(id, child) ->
            writeSTRef (result child) Link {link = value root, id}
        where
          collect = do
            stack <- readSTRef stackRef
            let w = head stack
            writeSTRef stackRef $! tail stack
            writeSTRef (onStack w) False
            if value v /= value w
              then
                NonEmpty.cons w <$> collect
              else
                pure $ w :| []
      preconnect v = do
        indexv <- readSTRef (index v)
        when (indexv == negate 1) $ strongconnect v
  pure preconnect

-- |
-- Applicative representing lazy side effects ala the R language. This
-- following law holds:
-- > a *> b = b
newtype LazyIO a = LazyIO {runLazyIO :: IO a}

instance Functor LazyIO where
  fmap = liftA

instance Applicative LazyIO where
  pure a = LazyIO (pure a)
  LazyIO f <*> LazyIO m =
    LazyIO $ do
      f <- unsafeInterleaveIO f
      m <- unsafeInterleaveIO m
      pure (f m)

newtype Index a i = Index
  { (!) :: forall x. a x -> i -> x
  }

tarjan :: (Traversable t, Ord k) => Index t i -> (k -> [i]) -> t k -> t (Component k)
tarjan Index {(!)} childrenRaw nodesRaw = unsafePerformIO $ do
  nodes <- runLazyIO $ traverse (LazyIO . stToIO . initialize) nodesRaw
  let children index = (nodes !) <$> childrenRaw index
  visit <- stToIO $ visiter children
  runLazyIO $ for nodes $ \node -> LazyIO $ stToIO $ do
    visit node
    readSTRef (result node)
