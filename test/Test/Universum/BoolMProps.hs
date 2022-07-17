module Test.Universum.BoolMProps
  ( hprop_andM
  , hprop_orM
  ) where

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Universum as U

genBoolList :: Gen [U.Bool]
genBoolList = Gen.list (Range.linear 0 1000) Gen.bool

hprop_andM :: Property
hprop_andM = property $ do
    bs <- forAll genBoolList
    U.andM (return <$> bs) === ((return $ U.and bs) :: U.Maybe U.Bool)

hprop_orM :: Property
hprop_orM = property $ do
    bs <- forAll genBoolList
    U.orM (return <$> bs) === ((return $ U.or bs) :: U.Maybe U.Bool)
