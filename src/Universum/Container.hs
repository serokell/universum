{-# LANGUAGE Safe #-}

-- | This module exports all container-related stuff.

module Universum.Container
       ( module Universum.Container.Class
       , module Universum.Container.Reexport
       ) where

import Universum.Container.Class hiding (checkingNotNull)
import Universum.Container.Reexport
