{-|
Module      : $header$
Description : Overloaded writing for String-like types
Copyright   : (c) Serokell 2018
License     : MIT
Maintainer  : Serokell <hi@serokell.io>
Stability   : experimental
Portability : portable

Generalizes the 'Base.putStr' family of functions for String likes such as lazy
and strict versions of 'Text' and 'ByteString'.

A caveat to using overloaded functions is that printing string literals raises
ambiguity errors in the presence of @OverloadedStrings@. To avoid this problem
wither add a type annotation @putStr ("Hello World!" :: Text)@ or use one of the
type constrained functions 'putText', 'putLText' etc.

You may add support for your own types by importing "Universum.Print.Internal"
and implementing 'Print'. However be advised that only the functions in this
module should be considered stable, not the interface for 'Print'.
-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}

module Universum.Print
       ( putStr
       , putStrLn
       , print
       , Print
       , putText
       , putTextLn
       , putLText
       , putLTextLn
       -- ** Writing strings to an arbitrary 'Handle'
       , hPutStr
       , hPutStrLn
       , hPrint
       ) where

import Data.Function ((.))

import Universum.Monad.Reexport (MonadIO, liftIO)

import Universum.Print.Internal (Print)
import qualified Universum.Print.Internal as I (hPutStrLn, hPutStr)

import qualified Prelude (print)
import qualified System.IO as SIO (Handle, hPrint)

import qualified Data.Text as T

import qualified Data.Text.Lazy as TL

import qualified Universum.Base as Base

-- | Write a string like value @a@ to a supplied 'SIO.Handle'.
hPutStr :: (Print a, MonadIO m) => SIO.Handle -> a -> m ()
hPutStr h = liftIO . I.hPutStr h
{-# SPECIALIZE hPutStr :: Print a => SIO.Handle -> a -> Base.IO () #-}

-- | Write a string like value @a@ to a supplied 'SIO.Handle', appending a
-- newline character.
hPutStrLn :: (Print a, MonadIO m) => SIO.Handle -> a -> m ()
hPutStrLn h = liftIO . I.hPutStrLn h
{-# SPECIALIZE hPutStrLn :: Print a => SIO.Handle -> a -> Base.IO () #-}

-- | Write a string like value to @stdout@/.
putStr :: (Print a, MonadIO m) => a -> m ()
putStr = hPutStr Base.stdout
{-# SPECIALIZE putStr :: Print a => a -> Base.IO () #-}

-- | Write a string like value to @stdout@ appending a newline character.
putStrLn :: (Print a, MonadIO m) => a -> m ()
putStrLn = hPutStrLn Base.stdout
{-# SPECIALIZE putStrLn :: Print a => a -> Base.IO () #-}

-- | Lifted version of 'Prelude.print'.
print :: forall a m . (MonadIO m, Base.Show a) => a -> m ()
print = liftIO . Prelude.print
{-# SPECIALIZE print :: Base.Show a => a -> Base.IO () #-}

-- | Lifted version of 'System.IO.hPrint'
hPrint :: (MonadIO m, Base.Show a) => SIO.Handle -> a -> m ()
hPrint h = liftIO . SIO.hPrint h
{-# SPECIALIZE hPrint :: Base.Show a => SIO.Handle -> a -> Base.IO () #-}

-- | Specialized to 'T.Text' version of 'putStr' or forcing type inference.
putText :: MonadIO m => T.Text -> m ()
putText = putStr
{-# SPECIALIZE putText :: T.Text -> Base.IO () #-}

-- | Specialized to 'T.Text' version of 'putStrLn' or forcing type inference.
putTextLn :: MonadIO m => T.Text -> m ()
putTextLn = putStrLn
{-# SPECIALIZE putTextLn :: T.Text -> Base.IO () #-}

-- | Specialized to 'TL.Text' version of 'putStr' or forcing type inference.
putLText :: MonadIO m => TL.Text -> m ()
putLText = putStr
{-# SPECIALIZE putLText :: TL.Text -> Base.IO () #-}

-- | Specialized to 'TL.Text' version of 'putStrLn' or forcing type inference.
putLTextLn :: MonadIO m => TL.Text -> m ()
putLTextLn = putStrLn
{-# SPECIALIZE putLTextLn :: TL.Text -> Base.IO () #-}
