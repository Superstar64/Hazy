module Control.Applicative.Combinators.NonEmpty
  ( some,
    endBy1,
    someTill,
    sepBy1,
    sepEndBy1,
  )
where

import Control.Applicative (Alternative)
import Data.List.NonEmpty (NonEmpty)
import Hazy.Prelude (placeholder)

some :: (Alternative m) => m a -> m (NonEmpty a)
some = placeholder

endBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
endBy1 = placeholder

someTill :: (Alternative m) => m a -> m end -> m (NonEmpty a)
someTill = placeholder

sepBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepBy1 = placeholder

sepEndBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepEndBy1 = placeholder
