{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Unsafe       #-}

-- | Reexports from @GHC.*@ modules of the <https://hackage.haskell.org/package/base> package.

module Universum.Base
       ( -- * Base types
         module Data.Bits
       , module Data.Char
       , module Data.Int
       , module Data.Word
       , Natural

         -- * Base type classes
       , module Data.Eq
       , module Data.Foldable
       , module Data.Ord
       , module Data.Traversable

         -- * System IO
       , module System.IO

         -- * Base GHC types
       , module Data.Proxy
       , module Data.Typeable
       , module Data.Void

       , module GHC.Base
       , module GHC.Enum
       , module GHC.Exts
       , module GHC.Float
       , module GHC.Generics
       , module GHC.Num
       , module GHC.Real
       , module GHC.Show
       , module GHC.TypeNats
       , module GHC.Types
       , module GHC.OverloadedLabels
       , module GHC.ExecutionStack
       , module GHC.Stack

         -- * Data.Kind
       , Constraint
       , Type

       , ($!)
       ) where

-- Base types
import Data.Bits (xor)
import Data.Char (chr)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import Numeric.Natural (Natural)

-- IO
import System.IO (FilePath, Handle, IOMode (..), stderr, stdin, stdout)

-- Base typeclasses
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, concat, concatMap, foldlM, foldrM, maximumBy, minimumBy)
import Data.Kind (Constraint, Type)
import Data.Ord (Down (..), Ord (..), Ordering (..), comparing)
import Data.Traversable (Traversable (..), fmapDefault, foldMapDefault, forM, mapAccumL, mapAccumR)

-- Base GHC types
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd, vacuous)

import GHC.Base (String, asTypeOf, maxInt, minInt, ord, seq)
import GHC.Enum (Bounded (..), Enum (..), boundedEnumFrom, boundedEnumFromThen)
import GHC.ExecutionStack (getStackTrace, showStackTrace)
import GHC.Exts (FunPtr, Ptr)
import GHC.Float (Double (..), Float (..), Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, logBase, pi, sin, sinh, sqrt, tan, tanh, (**)))
import GHC.Generics (Generic)
import GHC.Num (Integer, Num (..), subtract)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Real hiding (showSigned, (%))
import GHC.Show (Show)
import GHC.Stack (CallStack, HasCallStack, callStack, currentCallStack, getCallStack,
                  prettyCallStack, prettySrcLoc, withFrozenCallStack)
import GHC.TypeNats (CmpNat, KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import GHC.Types (Bool, Char, Coercible, IO, Int, Ordering, Word)

-- $setup
-- >>> import Universum.Function (const, ($))

-- | Stricter version of 'Data.Function.$' operator.
-- Default Prelude defines this at the toplevel module, so we do as well.
--
-- >>> const 3 $ Prelude.undefined
-- 3
-- >>> const 3 $! Prelude.undefined
-- *** Exception: Prelude.undefined
-- ...
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx
infixr 0 $!
