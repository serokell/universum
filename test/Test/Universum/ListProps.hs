module Test.Universum.ListProps
  ( hprop_ordNubCorrect
  , hprop_hashNubCorrect
  , hprop_sortNubCorrect
  , hprop_unstableNubCorrect
  ) where

import Data.List (nub)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Universum as U

genIntList :: Gen [U.Int]
genIntList = Gen.list (Range.linear 0 10000) Gen.enumBounded

hprop_ordNubCorrect :: Property
hprop_ordNubCorrect = property $ do
    xs <- forAll genIntList
    U.ordNub xs === nub xs

hprop_hashNubCorrect :: Property
hprop_hashNubCorrect = property $ do
    xs <- forAll genIntList
    U.hashNub xs === nub xs

hprop_sortNubCorrect :: Property
hprop_sortNubCorrect = property $ do
    xs <- forAll genIntList
    U.sortNub xs === (U.sort $ nub xs)

hprop_unstableNubCorrect :: Property
hprop_unstableNubCorrect = property $ do
    xs <- forAll genIntList
    (U.sort $ U.unstableNub xs) === (U.sort $ nub xs)
