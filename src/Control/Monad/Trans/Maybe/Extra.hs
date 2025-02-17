module Control.Monad.Trans.Maybe.Extra (
  hoistMaybe
) where

import Control.Monad.Trans.Maybe

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
