{-# LANGUAGE Safe #-}

-- | This module contains safe functions to work with list type (mostly with 'NonEmpty').

module Universum.List.Safe
       ( uncons
       , whenNotNull
       , whenNotNullM
       , foldr1
       , foldl1
       , minimum
       , maximum
       , minimumBy
       , maximumBy
       ) where

import qualified Data.Foldable as F
import Data.Ord (Ord, Ordering)

import Universum.Applicative (Applicative, pass)
import Universum.List.Reexport (NonEmpty (..))
import Universum.Monad (Maybe (..), Monad (..))

-- $setup
-- >>> import Universum.Applicative (pure)
-- >>> import Universum.Base ((==), (+), even)
-- >>> import Universum.Bool (Bool (..), not)
-- >>> import Universum.Container (length)
-- >>> import Universum.Function (($))
-- >>> import Universum.Print (print)

-- | Destructuring list into its head and tail if possible. This function is total.
--
-- >>> uncons []
-- Nothing
-- >>> uncons [1..5]
-- Just (1,[2,3,4,5])
-- >>> uncons (5 : [1..5]) >>= \(f, l) -> pure $ f == length l
-- Just True
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

{- | Performs given action over 'NonEmpty' list if given list is non empty.

>>> whenNotNull [] $ \(b :| _) -> print (not b)
>>> whenNotNull [False,True] $ \(b :| _) -> print (not b)
True

-}
whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
whenNotNull []     _ = pass
whenNotNull (x:xs) f = f (x :| xs)
{-# INLINE whenNotNull #-}

-- | Monadic version of 'whenNotNull'.
whenNotNullM :: Monad m => m [a] -> (NonEmpty a -> m ()) -> m ()
whenNotNullM ml f = ml >>= \l -> whenNotNull l f
{-# INLINE whenNotNullM #-}

-- | A variant of 'foldl' that has no base case, and thus may only be
-- applied to 'NonEmpty'.
--
-- >>> foldl1 (+) (1 :| [2,3,4,5])
-- 15
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 = F.foldl1
{-# INLINE foldl1 #-}

-- | A variant of 'foldr' that has no base case, and thus may only be
-- applied to 'NonEmpty'.
--
-- >>> foldr1 (+) (1 :| [2,3,4,5])
-- 15
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = F.foldr1
{-# INLINE foldr1 #-}

-- | The least element of a 'NonEmpty' with respect to the given
-- comparison function.
minimumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy = F.minimumBy
{-# INLINE minimumBy #-}

-- | The least element of a 'NonEmpty'.
--
-- >>> minimum (1 :| [2,3,4,5])
-- 1
minimum :: Ord a => NonEmpty a -> a
minimum = F.minimum
{-# INLINE minimum #-}

-- | The largest element of a 'NonEmpty' with respect to the given
-- comparison function.
maximumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy = F.maximumBy
{-# INLINE maximumBy #-}

-- | The largest element of a 'NonEmpty'.
--
-- >>> maximum (1 :| [2,3,4,5])
-- 5
maximum :: Ord a => NonEmpty a -> a
maximum = F.maximum
{-# INLINE maximum #-}
