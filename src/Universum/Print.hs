{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}

-- | Generalization of 'Prelude.putStr' and 'Prelude.putStrLn' functions.

module Universum.Print
       ( Print (..)
       , putStr
       , putStrLn
       , print
       , putText
       , putTextLn
       , putLText
       , putLTextLn
       ) where

import Data.Function ((.))

import Universum.Monad.Reexport (MonadIO, liftIO)

import qualified Prelude (print)
import qualified System.IO as SIO (hPutStr, hPutStrLn, Handle)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Universum.Base as Base

-- | Polymorfic over string and lifted to 'MonadIO' printing functions.
class Print a where
  hPutStr :: MonadIO m => SIO.Handle -> a -> m ()
  hPutStrLn :: MonadIO m => SIO.Handle -> a -> m ()

instance Print T.Text where
  hPutStr h = liftIO . T.hPutStr h
  hPutStrLn h = liftIO . T.hPutStrLn h

instance Print TL.Text where
  hPutStr h = liftIO . TL.hPutStr h
  hPutStrLn h = liftIO . TL.hPutStrLn h

instance Print BS.ByteString where
  hPutStr h = liftIO . BS.hPutStr h
  hPutStrLn h = liftIO . BS.hPutStrLn h

instance Print BL.ByteString where
  hPutStr h = liftIO . BL.hPutStr h
  hPutStrLn h = liftIO . BL.hPutStrLn h

instance Print [Base.Char] where
  hPutStr h = liftIO . SIO.hPutStr h
  hPutStrLn h = liftIO . SIO.hPutStrLn h


putStr :: (Print a, MonadIO m) => a -> m ()
putStr = hPutStr Base.stdout

putStrLn :: (Print a, MonadIO m) => a -> m ()
putStrLn = hPutStrLn Base.stdout

-- | Lifted version of 'Prelude.print'.
print :: forall a m . (MonadIO m, Base.Show a) => a -> m ()
print = liftIO . Prelude.print

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
