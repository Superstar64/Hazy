module Stage2.Resolve.Stability where

import Error (unstableImports)
import Stage1.Position (Position)

data Stability
  = Ignore
  | Stable [Position]
  | Unstable Position
  deriving (Show)

instance Semigroup Stability where
  Ignore <> stability = stability
  stability <> Ignore = stability
  Stable positions <> Stable positions' = Stable (positions ++ positions')
  Stable positions <> Unstable position = unstableImports (positions ++ [position])
  Unstable position <> Stable positions = unstableImports (position : positions)
  Unstable position <> Unstable position' = unstableImports [position, position']

instance Monoid Stability where
  mempty = Ignore
