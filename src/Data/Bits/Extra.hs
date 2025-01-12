module Data.Bits.Extra (
  (.&&.)
) where

import Data.Bits

infix 6 .&&.

-- Return true if `x` has all bit flag set in `y`.
(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = x .&. y == y
