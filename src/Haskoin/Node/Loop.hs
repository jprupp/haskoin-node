-- | A replacement for 'Control.Monad.forever' that stays a flat loop in any
-- monad.
--
-- @forever a = let a' = a *> a' in a'@ relies on @*>@ being a tail call. When
-- the monad is only known through a dictionary (a function polymorphic in
-- @m@ that GHC did not specialise), @*>@ is whatever the instance defines.
-- monad-logger's @LoggingT@ leaves it at the default @(id <$ a) <*> b@, and
-- @ReaderT@ defines @>>@ as @*>@, so every iteration leaves an @<*>@
-- continuation frame and two thunks behind for the lifetime of the loop.
--
-- This version sequences with @>>=@ only, which is a tail call in all these
-- stacks, so it never accumulates.
module Haskoin.Node.Loop (loopForever) where

loopForever :: (Monad m) => m a -> m b
loopForever action = go
  where
    go = action >>= \_ -> go
{-# INLINE loopForever #-}
