{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE Trustworthy        #-}
#if ( __GLASGOW_HASKELL__ >= 804 )
{-# LANGUAGE TypeInType         #-}
#endif

-- | Functions for debugging. If you left these functions in your code
-- then warning is generated to remind you about left usages. Also, some
-- functions (and data types) are convenient for prototyping.

module Universum.Debug
       ( Undefined (..)
       , error
       , trace
       , traceM
       , traceId
       , traceIdWith
       , traceShow
       , traceShowId
       , traceShowIdWith
       , traceShowM
       , undefined
       ) where

import Control.Monad (Monad, return)
import Data.Data (Data)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

#if ( __GLASGOW_HASKELL__ >= 800 )
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)

import Universum.Base (HasCallStack, callStack)
#endif

import Universum.Applicative (pass)
import Universum.Print (putStrLn)

import qualified Prelude as P

-- | Version of 'Debug.Trace.trace' that leaves a warning and takes 'Text'.
{-# WARNING trace "'trace' remains in code" #-}
trace :: Text -> a -> a
trace string expr = unsafePerformIO (do
    putStrLn string
    return expr)

-- | 'P.error' that takes 'Text' as an argument.
#if ( __GLASGOW_HASKELL__ >= 800 )
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack
      => Text -> a
error s = raise# (errorCallWithCallStackException (unpack s) callStack)
#else
error :: Text -> a
error s = P.error (unpack s)
#endif

-- | Version of 'Debug.Trace.traceShow' that leaves a warning.
{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> b -> b
traceShow a b = trace (pack (P.show a)) b

-- | Version of 'Debug.Trace.traceShowId' that leaves a warning.
{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = trace (pack (P.show a)) a

{- | Version of 'Debug.Trace.traceId' that leaves a warning.
Useful to tag printed data, for instance:

@
traceIdWith (\x -> "My data: " <> show x) (veryLargeExpression)
@

This is especially useful with custom formatters:

@
traceIdWith (\x -> "My data: " <> pretty x) (veryLargeExpression)
@
-}
{-# WARNING traceIdWith "'traceIdWith' remains in code" #-}
traceIdWith :: (a -> Text) -> a -> a
traceIdWith f a = trace (f a) a

-- | Version of 'Debug.Trace.traceShowId' that leaves a warning.
-- Useful to tag printed data, for instance:
--
-- @
-- traceShowIdWith ("My data: ", ) (veryLargeExpression)
-- @
{-# WARNING traceShowIdWith "'traceShowIdWith' remains in code" #-}
traceShowIdWith :: P.Show s => (a -> s) -> a -> a
traceShowIdWith f a = trace (pack (P.show (f a))) a

-- | Version of 'Debug.Trace.traceShowM' that leaves a warning.
{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, Monad m) => a -> m ()
traceShowM a = trace (pack (P.show a)) pass

-- | Version of 'Debug.Trace.traceM' that leaves a warning and takes 'Text'.
{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => Text -> m ()
traceM s = trace s pass

-- | Version of 'Debug.Trace.traceId' that leaves a warning.
{-# WARNING traceId "'traceId' remains in code" #-}
traceId :: Text -> Text
traceId s = trace s s

-- | Similar to 'undefined' but data type.
{-# WARNING Undefined "'Undefined' type remains in code" #-}
data Undefined = Undefined
    deriving (P.Eq, P.Ord, P.Show, P.Read, P.Enum, P.Bounded, Data, Generic)

-- | 'P.undefined' that leaves a warning in code on every usage.
{-# WARNING undefined "'undefined' function remains in code (or use 'error')" #-}
#if ( __GLASGOW_HASKELL__ >= 800 )
undefined :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => a
#else
undefined :: a
#endif
undefined = P.undefined
