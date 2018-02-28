-- | This module reexports functionality regarding operations with functions
-- (composition, application, etc.)

module Universum.Function
       ( module Data.Function
       , identity
       ) where

import Data.Function (const, fix, flip, id, on, ($), (.))

-- | Renamed version of 'Prelude.id'.
identity :: a -> a
identity = id
{-# INLINE identity #-}
