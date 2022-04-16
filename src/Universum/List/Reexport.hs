{-# LANGUAGE Trustworthy #-}

-- | This module reexports functinons to work with list, 'NonEmpty' and String types.

module Universum.List.Reexport
       ( module Data.List
       , module Data.List.NonEmpty
       , module GHC.Exts
       ) where

import Data.List (break, cycle, drop, dropWhile, filter, genericDrop, genericLength,
                  genericReplicate, genericSplitAt, genericTake, inits, intercalate,
                  intersperse, isPrefixOf, iterate, permutations, repeat, replicate, reverse, scanl,
                  scanr, sort, sortBy, sortOn, splitAt, subsequences, tails, take, takeWhile,
                  transpose, unfoldr, unzip, unzip3, zip, zip3, zipWith, (++))
import Data.List.NonEmpty (NonEmpty (..), group, groupAllWith, groupBy, groupWith, head, init, last, nonEmpty, tail)

import GHC.Exts (sortWith)
