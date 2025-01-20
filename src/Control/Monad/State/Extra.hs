module Control.Monad.State.Extra (
  update,
  update',

  getNext
) where

import Control.Monad.State

-- Modify the current state and return it's previous value
update :: MonadState s m => (s -> s) -> m s
update f = do
  s <- get
  modify f
  return s

-- A strict version of `update`
update' :: MonadState s m => (s -> s) -> m s
update' f = do
  s <- get
  modify' f
  return s

getNext :: (MonadState s m, Enum s) => m s
getNext = update succ
