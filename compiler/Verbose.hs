-- Lazy verboseness
module Verbose
  ( Verbose,
    runVerbose,
    Debug (..),
  )
where

import Control.Monad (ap, liftM)
import Control.Monad.Fix (MonadFix (..))
import Data.Functor.Identity (Identity)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder, fromLazyText, fromString, toLazyText)
import qualified Data.Text.Lazy.IO as Text.IO
import System.IO.Unsafe (unsafeInterleaveIO)

-- |
-- Monad with debugging verbosity This is a nondeterministic monad. So the
-- following law must hold modulo printing statements:
-- > a >> b = b
class (MonadFix m) => Debug m where
  parsing :: Builder -> a -> m a
  creatingIndexes :: Builder -> m a -> m a
  resolving :: Builder -> a -> m a

instance Debug Identity where
  parsing _ = pure
  creatingIndexes _ = id
  resolving _ = pure

newtype Verbose a = Verbose {runVerbose' :: Lazy.Text -> IO a}

runVerbose :: Verbose a -> IO a
runVerbose (Verbose v) = v (Lazy.pack "")

instance Functor Verbose where
  fmap = liftM

instance Applicative Verbose where
  pure = Verbose . pure . pure
  (<*>) = ap

instance Monad Verbose where
  Verbose run >>= after = Verbose $ \path -> do
    a <- unsafeInterleaveIO $ run path
    runVerbose' (after a) path

instance MonadFix Verbose where
  mfix go = Verbose $ \path -> do
    mfix $ \value -> runVerbose' (go value) path

instance Debug Verbose where
  parsing text value = over text $ with (message $ fromString "Parsing") value
  creatingIndexes text value = over text $ do
    value <- value
    with (message $ fromString "Creating Indexes") value
  resolving text = with (message $ fromString "Resolving " <> text)

over :: Builder -> Verbose a -> Verbose a
over modulex (Verbose run) = Verbose $ \_ -> run (toLazyText $ modulex <> fromString ":")

message :: Builder -> Verbose ()
message text = Verbose $ \path ->
  do
    let message = toLazyText $ fromLazyText path <> text
    Text.IO.putStrLn message

with :: Verbose () -> a -> Verbose a
with print a = seq <$> print <*> pure a
