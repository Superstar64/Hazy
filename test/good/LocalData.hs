module LocalData where

id :: forall a. a -> a
id = runWrap . Wrap where
  data Wrap = Wrap { runWrap :: a }
