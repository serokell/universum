{-# LANGUAGE Safe #-}

-- | This module reexports functions to work with monads.

module Universum.Monad.Reexport
       ( -- * Reexport transformers
         module Control.Monad.Except
       , module Control.Monad.Reader
       , module Control.Monad.State.Strict
       , module Control.Monad.Trans
       , module Control.Monad.Trans.Identity
       , module Control.Monad.Trans.Maybe

         -- * Reexport Maybe
       , module Data.Maybe

         -- * Reexport Either
       , module Data.Either

       , Monad ((>>=), (>>), return)
       , MonadFail (fail)
       , MonadPlus (..)

       , (=<<)
       , (>=>)
       , (<=<)
       , forever

       , join
       , mfilter
       , filterM
       , mapAndUnzipM
       , zipWithM
       , zipWithM_
       , foldM
       , foldM_
       , replicateM
       , replicateM_

       , liftM2
       , liftM3
       , liftM4
       , liftM5
       , ap

       , (<$!>)
       ) where

-- Monad transformers
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (MonadReader, Reader, ReaderT (..), ask, asks, local, reader, runReader)
import Control.Monad.State.Strict (MonadState, State, StateT (..), evalState, evalStateT, execState,
                                   execStateT, get, gets, modify, modify', put, runState, state,
                                   withState)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Trans.Identity (IdentityT (runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, maybeToExceptT)

-- Maybe
import Data.Maybe (Maybe (..), catMaybes, fromMaybe, isJust, isNothing, mapMaybe,
                   maybe, maybeToList)

-- Either
import Data.Either (Either (..), either, isLeft, isRight, lefts, partitionEithers, rights)

import Control.Monad hiding (fail)
import Control.Monad.Fail (MonadFail (..))
