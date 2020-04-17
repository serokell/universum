{-# LANGUAGE Safe #-}

-- | Lifted versions of functions working with files and common IO.
-- All functions are specialized to 'Data.Text.Text'.

module Universum.Lifted.File
       ( appendFile
       , getLine
       , readFile
       , writeFile
       , withFile
       , openFile
       , hClose
       ) where

import Control.Exception.Safe (MonadMask, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Function ((.))
import Data.Text (Text)
import Prelude (FilePath)
import System.IO (Handle, IOMode)

import qualified Data.Text.IO as XIO
import qualified System.IO as XIO (IO, hClose, openFile)

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

-- | Lifted version of 'Data.Text.IO.appendFile'.
appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile a b = liftIO (XIO.appendFile a b)
{-# INLINE appendFile #-}

-- | Lifted version of 'Data.Text.IO.getLine'.
getLine :: MonadIO m => m Text
getLine = liftIO XIO.getLine
{-# INLINE getLine #-}

-- | Lifted version of 'Data.Text.IO.readFile'.
readFile :: MonadIO m => FilePath -> m Text
readFile a = liftIO (XIO.readFile a)
{-# INLINE readFile #-}

-- | Lifted version of 'Data.Text.IO.writeFile'.
writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile a b = liftIO (XIO.writeFile a b)
{-# INLINE writeFile #-}

-- | Lifted version of 'System.IO.openFile'.
--
-- See also 'withFile' for more information.
openFile :: MonadIO m => FilePath -> IOMode -> m Handle
openFile a b = liftIO (XIO.openFile a b)
{-# INLINE openFile #-}

-- | Close a file handle
--
-- See also 'withFile' for more information.
hClose :: MonadIO m => Handle -> m ()
hClose = liftIO . XIO.hClose
{-# INLINE hClose #-}

-- | Opens a file, manipulates it with the provided function and closes the
-- handle before returning. The 'Handle' can be written to using the
-- 'Universum.Print.hPutStr' and 'Universum.Print.hPutStrLn' functions.
--
-- 'withFile' is essentially the 'bracket' pattern, specialized to files. This
-- should be preferred over 'openFile' + 'hClose' as it properly deals with
-- (asynchronous) exceptions. In cases where 'withFile' is insufficient, for
-- instance because the it is not statically known when manipulating the
-- 'Handle' has finished, one should consider other safe paradigms for resource
-- usage, such as the @ResourceT@ transformer from the @resourcet@ package,
-- before resorting to 'openFile' and 'hClose'.
withFile :: (MonadIO m, MonadMask m) => FilePath -> IOMode -> (Handle -> m a) -> m a
withFile filePath mode f = bracket (openFile filePath mode) hClose f

{-# SPECIALIZE appendFile :: FilePath -> Text -> XIO.IO () #-}
{-# SPECIALIZE getLine :: XIO.IO Text #-}
{-# SPECIALIZE readFile :: FilePath -> XIO.IO Text #-}
{-# SPECIALIZE writeFile :: FilePath -> Text -> XIO.IO () #-}
{-# SPECIALIZE openFile :: FilePath -> IOMode -> XIO.IO Handle #-}
{-# SPECIALIZE hClose :: Handle -> XIO.IO () #-}
{-# SPECIALIZE withFile :: FilePath -> IOMode -> (Handle -> XIO.IO a) -> XIO.IO a #-}
