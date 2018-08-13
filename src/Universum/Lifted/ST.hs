-- | This module contains lifted version of 'XIO.stToIO' function.

module Universum.Lifted.ST {-# DEPRECATED "This module will be removed in a future version of this package, use `liftIO` directly with functions from `Control.Monad.ST` instead." #-}
       ( stToIO
       ) where

import Control.Monad.Trans (MonadIO, liftIO)

import qualified Control.Monad.ST as XIO

-- | Lifted version of 'XIO.stToIO'.
stToIO :: MonadIO m => XIO.ST XIO.RealWorld a -> m a
stToIO a = liftIO (XIO.stToIO a)
{-# INLINE stToIO #-}
