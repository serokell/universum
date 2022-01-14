{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Re-exports most useful functionality from 'safe-exceptions'. Also
-- provides some functions to work with exceptions over 'MonadError'.

module Universum.Exception
       ( module Control.Exception.Safe
       , Bug (..)
       , bug
       , pattern Exc
       , validateJust
       ) where

-- exceptions from safe-exceptions
import Control.Exception.Safe (Exception (..), MonadCatch, MonadMask (..), MonadThrow,
                               SomeException (..), bracket, bracketOnError, bracket_, catch,
                               catchAny, displayException, finally, handleAny, onException, throwM,
                               try, tryAny)
import Control.Monad.Except (MonadError, throwError)
import Universum.Applicative (Applicative (pure))
import Universum.Monad (Maybe (..), maybe)

import Data.List ((++))
import GHC.Show (Show)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import qualified Control.Exception.Safe as Safe (displayException, impureThrow, toException)

-- | Type that represents exceptions used in cases when a particular codepath
-- is not meant to be ever executed, but happens to be executed anyway.
data Bug = Bug SomeException CallStack
    deriving stock (Show)

instance Exception Bug where
    displayException (Bug e cStack) = Safe.displayException e ++ "\n"
                                   ++ prettyCallStack cStack

-- | Generate a pure value which, when forced, will synchronously
-- throw the exception wrapped into 'Bug' data type.
bug :: (HasCallStack, Exception e) => e -> a
bug e = Safe.impureThrow (Bug (Safe.toException e) callStack)

-- To suppress redundant applicative constraint warning on GHC 8.0
-- | Throws error for 'Maybe' if 'Data.Maybe.Nothing' is given.
-- Operates over 'MonadError'.
validateJust :: (MonadError e m) => e -> Maybe a -> m a
validateJust err = maybe (throwError err) pure


{- | Pattern synonym to easy pattern matching on exceptions. So intead of
writing something like this:

@
isNonCriticalExc e
    | Just (_ :: NodeAttackedError) <- fromException e = True
    | Just DialogUnexpected{} <- fromException e = True
    | otherwise = False
@

you can use 'Exc' pattern synonym:

@
isNonCriticalExc = \case
    Exc (_ :: NodeAttackedError) -> True  -- matching all exceptions of type 'NodeAttackedError'
    Exc DialogUnexpected{} -> True
    _ -> False
@

This pattern is bidirectional. You can use @Exc e@ instead of @toException e@.
-}
pattern Exc :: Exception e => e -> SomeException
pattern Exc e <- (fromException -> Just e)
  where
    Exc e = toException e
