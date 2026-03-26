module Control.Applicative.Combinators
  ( (<|>),
    many,
    some,
    optional,
    empty,
    between,
    choice,
    count,
    count,
    eitherP,
    endBy,
    endBy1,
    manyTill,
    manyTill_,
    someTill,
    someTill_,
    option,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    skipMany,
    skipSome,
    skipCount,
    skipManyTill,
    skipSomeTill,
  )
where

import Control.Applicative (Alternative (empty, many, some, (<|>)), optional)
import Hazy.Prelude (placeholder)

between :: (Applicative m) => m open -> m close -> m a -> m a
between = placeholder

choice :: (Foldable f, Alternative m) => f (m a) -> m a
choice = placeholder

count :: (Applicative m) => Int -> m a -> m [a]
count = placeholder

count' :: (Alternative m) => Int -> Int -> m a -> m [a]
count' = placeholder

eitherP :: (Alternative m) => m a -> m b -> m (Either a b)
eitherP = placeholder

endBy :: (Alternative m) => m a -> m sep -> m [a]
endBy = placeholder

endBy1 :: (Alternative m) => m a -> m sep -> m [a]
endBy1 = placeholder

manyTill :: (Alternative m) => m a -> m end -> m [a]
manyTill = placeholder

manyTill_ :: (Alternative m) => m a -> m end -> m ([a], end)
manyTill_ = placeholder

someTill :: (Alternative m) => m a -> m end -> m [a]
someTill = placeholder

someTill_ :: (Alternative m) => m a -> m end -> m ([a], end)
someTill_ = placeholder

option :: (Alternative m) => a -> m a -> m a
option = placeholder

sepBy :: (Alternative m) => m a -> m sep -> m [a]
sepBy = placeholder

sepBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepBy1 = placeholder

sepEndBy :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy = placeholder

sepEndBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy1 = placeholder

skipMany :: (Alternative m) => m a -> m ()
skipMany = placeholder

skipSome :: (Alternative m) => m a -> m ()
skipSome = placeholder

skipCount :: (Applicative m) => Int -> m a -> m ()
skipCount = placeholder

skipManyTill :: (Alternative m) => m a -> m end -> m end
skipManyTill = placeholder

skipSomeTill :: (Alternative m) => m a -> m end -> m end
skipSomeTill = placeholder
