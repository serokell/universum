{-|
Module      : $header$
Description : Class machinery for overloaded writing of String-like types
Copyright   : (c) Justus Adam 2018
License     : MIT
Maintainer  : Serokell <hi@serokell.io>
Stability   : experimental
Portability : portable

You may import this module to define your own, custom instances of 'Print'. Be
advised however that this module is an internal API and may be subject to change
__even for minor version increments__.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}

module Universum.Print.Internal (Print(..)) where

import qualified System.IO as SIO (Handle, hPutStr, hPutStrLn)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Universum.Base as Base

-- | Support class to overload writing of string like values.
class Print a where
  hPutStr :: SIO.Handle -> a -> Base.IO ()
  hPutStrLn :: SIO.Handle -> a -> Base.IO ()

instance Print T.Text where
  hPutStr = T.hPutStr
  hPutStrLn = T.hPutStrLn

instance Print TL.Text where
  hPutStr = TL.hPutStr
  hPutStrLn = TL.hPutStrLn

instance Print BS.ByteString where
  hPutStr = BS.hPutStr
  hPutStrLn = BS.hPutStrLn

instance Print BL.ByteString where
  hPutStr = BL.hPutStr
  hPutStrLn = BL.hPutStrLn

instance Print [Base.Char] where
  hPutStr = SIO.hPutStr
  hPutStrLn = SIO.hPutStrLn
