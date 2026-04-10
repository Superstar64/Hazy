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
import Data.STRef
  ( STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,
  )
import Data.Traversable (for)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

data Component k
  = Group [k]
  | Link k
  deriving (Eq, Show)

data State s k = State
  { index :: !(STRef s Int),
    lowlink :: !(STRef s Int),
    onStack :: !(STRef s Bool),
    value :: k,
    link :: !(STRef s (Component k))
  }

initialize :: k -> ST s (State s k)
initialize value = do
  index <- newSTRef $! negate 1
  lowlink <- newSTRef $! negate 1
  onStack <- newSTRef False
  link <- newSTRef undefined
  pure State {index, lowlink, onStack, value, link}

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
          component <- newSTRef []
          let run = do
                stack <- readSTRef stackRef
                let w = head stack
                writeSTRef stackRef $! tail stack
                writeSTRef (onStack w) False
                writeSTRef (link w) $! Link (value v)
                modifySTRef' component (value w :)
                loop w
              loop w =
                if value v /= value w
                  then run
                  else do
                    component <- readSTRef component
                    writeSTRef (link v) $! Group component
          run
      -- A pre strongly connect step is needed to make sure the result is always
      -- the same regardless of the evaluation order.
      preconnect v = do
        for_ (children $ value v) $ \w -> when (value v > value w) $ do
          indexw <- readSTRef (index w)
          when (indexw == negate 1) $ preconnect w
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
    readSTRef (link node)
