module Data.Bits.Extra (
  (.?.)
) where

import Data.Bits

infix 6 .?.

-- Return `True` if `x` has all the bits set in `y` also set.
(.?.) :: Bits a => a -> a -> Bool
x .?. y = x .&. y == y
