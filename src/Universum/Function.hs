{-# LANGUAGE Safe #-}

-- | This module reexports very basic and primitive functions and function combinators.

module Universum.Function
       ( module Data.Function
       ) where

import Data.Function (const, fix, flip, id, on, ($), (.), (&))
