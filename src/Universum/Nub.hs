{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

{-| Functions to remove duplicates from a list.

 = Performance
 To check the performance many benchmarks were done.
 Benchmarks were made on lists of 'Prelude.Int's and 'Data.Text.Text's.
 There were two types of list to use:

 * Lists which consist of many different elements

 * Lists which consist of many same elements


 Here are some recomendations for usage of particular functions based on benchmarking resutls.

 * 'hashNub' is faster than 'ordNub' when there're not so many different values in the list.

 * 'hashNub' is the fastest with 'Data.Text.Text'.

 * 'sortNub' has better performance than 'ordNub' but should be used when sorting is also needed.

 * 'unstableNub' has better performance than 'hashNub' but doesn't save the original order.
-}

module Universum.Nub
       ( hashNub
       , ordNub
       , sortNub
       , unstableNub
       ) where

#if !MIN_VERSION_hashable(1,4,0)
import Data.Eq (Eq)
#endif
import Data.Hashable (Hashable)
import Data.HashSet as HashSet
import Data.Ord (Ord)
import Prelude ((.))

import qualified Data.Set as Set

-- | Like 'Prelude.nub' but runs in @O(n * log n)@ time and requires 'Ord'.
--
-- >>> ordNub [3, 3, 3, 2, 2, -1, 1]
-- [3,2,-1,1]
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

-- | Like 'Prelude.nub' but runs in @O(n * log_16(n))@ time and requires 'Hashable'.
--
-- >>> hashNub [3, 3, 3, 2, 2, -1, 1]
-- [3,2,-1,1]
#if MIN_VERSION_hashable(1,4,0)
hashNub :: (Hashable a) => [a] -> [a]
#else
hashNub :: (Eq a, Hashable a) => [a] -> [a]
#endif
hashNub = go HashSet.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `HashSet.member` s
      then go s xs
      else x : go (HashSet.insert x s) xs

-- | Like 'ordNub' but also sorts a list.
--
-- >>> sortNub [3, 3, 3, 2, 2, -1, 1]
-- [-1,1,2,3]
sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList

-- | Like 'hashNub' but has better performance and also doesn't save the order.
--
-- >>> unstableNub [3, 3, 3, 2, 2, -1, 1]
-- [1,2,3,-1]
#if MIN_VERSION_hashable(1,4,0)
unstableNub :: (Hashable a) => [a] -> [a]
#else
unstableNub :: (Eq a, Hashable a) => [a] -> [a]
#endif
unstableNub = HashSet.toList . HashSet.fromList
