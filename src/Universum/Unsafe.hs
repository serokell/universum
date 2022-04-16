{-# LANGUAGE Unsafe #-}
{-# LANGUAGE LambdaCase #-}

{- | Unsafe functions to work with lists and 'Maybe'.
Sometimes unavoidable but better don't use them. This module
is intended to be imported qualified and it's not even included
in default prelude exports.

@
import qualified Universum.Unsafe as Unsafe

foo :: [a] -> a
foo = Unsafe.head
@

-}

module Universum.Unsafe
  ( head
  , tail
  , init
  , last
  , at
  , (!!)
  , fromJust
  , foldr1
  , foldl1
  , minimum
  , maximum
  , minimumBy
  , maximumBy
  ) where

import Data.Foldable (Foldable)
import Data.List ((++), foldr, foldl, foldl1', foldl')
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Ord ((<), Ord (min, max), Ordering (..))
import GHC.Base ((.), ($!))
import GHC.Err (error)
import GHC.Num ((-))

import Universum.Base (HasCallStack, Int, String)

-- | \(\mathcal{O}(1)\). Extract the first element of a list, which must be non-empty.
head :: HasCallStack => [a] -> a
head = \case
  [] -> errorEmptyList "head"
  (x : _) -> x

-- | \(\mathcal{O}(1)\). Extract the elements after the head of a list, which
-- must be non-empty.
tail :: HasCallStack => [a] -> [a]
tail = \case
  [] -> errorEmptyList "tail"
  (_ : xs) -> xs

-- | \(\mathcal{O}(n)\). Return all the elements of a list except the last one.
-- The list must be non-empty.
init :: HasCallStack => [a] -> [a]
init = \case
  [_] -> []
  (x : xs) -> x : init xs
  [] -> errorEmptyList "init"

-- | \(\mathcal{O}(n)\). Extract the last element of a list, which must be
-- finite and non-empty.
last :: HasCallStack => [a] -> a
last = \case
  [x] -> x
  (_ : xs) -> last xs
  [] -> errorEmptyList "last"

-- | List index (subscript) operator, starting from 0.
-- It is an instance of the more general 'Data.List.genericIndex',
-- which takes an index of any integral type.
(!!) :: HasCallStack => [a] -> Int -> a
_      !! n | n < 0 =  error "Universum.!!: negative index"
[]     !! _         =  error "Universum.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n - 1)

-- | A variant of 'foldr' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @'foldr1' f = 'List.foldr1' f . 'toList'@
foldr1 :: (HasCallStack, Foldable t) => (a -> a -> a) -> t a -> a
foldr1 f xs = fromMaybe (error "foldr1: empty structure") (foldr mf Nothing xs)
  where
    mf x m = Just (case m of
                      Nothing -> x
                      Just y  -> f x y)

-- | A variant of 'foldl' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @'foldl1' f = 'List.foldl1' f . 'toList'@
foldl1 :: (HasCallStack, Foldable t) => (a -> a -> a) -> t a -> a
foldl1 f xs = fromMaybe (error "foldl1: empty structure") (foldl mf Nothing xs)
  where
    mf m y = Just (case m of
                      Nothing -> y
                      Just x  -> f x y)

-- | The least element of a non-empty structure.
minimum :: (HasCallStack, Ord a) => [a] -> a
{-# INLINABLE minimum #-}
minimum = \case
  [] -> errorEmptyList "minimum"
  xs -> foldl1' min xs

-- | The largest element of a non-empty structure.
maximum :: (HasCallStack, Ord a) => [a] -> a
{-# INLINABLE maximum #-}
maximum = \case
  [] -> errorEmptyList "maximum"
  xs -> foldl1' max xs

-- | The least element of a non-empty structure with respect to the given comparison function.
minimumBy :: (HasCallStack, Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = fromMaybe (error "minimumBy: empty structure") . foldl' min' Nothing
  where
    min' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        GT -> y
        _ -> x
{-# INLINEABLE minimumBy #-}

-- | The largest element of a non-empty structure with respect to the given comparison function.
maximumBy :: (HasCallStack, Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = fromMaybe (error "maximumBy: empty structure") . foldl' max' Nothing
  where
    max' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        GT -> x
        _ -> y
{-# INLINEABLE maximumBy #-}

-- | Similar to '!!' but with flipped arguments.
{-@ at :: n : Nat -> {xs : NonEmptyList a | len xs > n} -> a @-}
at :: HasCallStack => Int -> [a] -> a
at n xs = xs !! n

errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun =
  error ("Universum." ++ fun ++ ": empty list")
