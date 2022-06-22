{-# LANGUAGE Trustworthy #-}

-- | Utility functions on top of 'Container' typeclass.

module Universum.Container.Utils
       ( groupByFst
       , groupByKey
       , groupByKeyBy
       ) where

import Data.Function (id, on, (.))
import Data.List.NonEmpty (NonEmpty (..))

import Universum.Base
import Universum.Container.Class

-- $setup
-- >>> import Data.Function
-- >>> import GHC.Num
-- >>> import Data.Text (toLower)

-- | Variation of 'groupByKey' that accepts the comparison function on keys
-- explicitly. This function must define a (computable) equivalence relation.
--
-- Among multiple keys appearing in the same group it will pick the leftmost
-- one as the representer of the equivalence class.
--
-- >>> groupByKeyBy (<=) id [(1, 10), (2, 11), (0, 12), (1, 13), (3, 14)]
-- [(1,10 :| [11]),(0,12 :| [13,14])]
groupByKeyBy
  :: Container t
  => (k -> k -> Bool) -> (Element t -> (k, v)) -> t -> [(k, NonEmpty v)]
groupByKeyBy kcmp split = start . toList
  where
    start [] = []
    start (a : as)
      | (k, v) <- split a
      , let (ys, zs) = go k as
      = (k, v :| ys) : zs

    go _ [] = ([], [])
    go ko (a : as)
      | (kn, v) <- split a
      = if ko `kcmp` kn
         then let (vs, ws) = go ko as
                 in (v : vs, ws)
         else let (vs, ws) = go kn as
                 in ([], (kn, v :| vs) : ws)

-- | Variation of 'groupByKey' that matches mapped keys on equality.
--
-- >>> groupByKeyOn toLower id [("A", 1), ("a", 2), ("b", 3)]
-- [("A",1 :| [2]),("b",3 :| [])]
--
-- @
-- 'groupByKeyOn' f ≡ 'groupByKeyBy' ((==) `on` f)
-- @
groupByKeyOn
  :: (Container t, Eq k')
  => (k -> k') -> (Element t -> (k, v)) -> t -> [(k, NonEmpty v)]
groupByKeyOn f = groupByKeyBy ((==) `on` f)

-- | Operates like 'groupByFst', but uses the provided getter
-- for the key to group by and for the value.
--
-- >>> groupByKey (\x -> (x `mod` 5, x)) [1, 6, 7, 2, 12, 11]
-- [(1,1 :| [6]),(2,7 :| [2,12]),(1,11 :| [])]
groupByKey
  :: (Container t, Eq k)
  => (Element t -> (k, v)) -> t -> [(k, NonEmpty v)]
groupByKey = groupByKeyBy (==)

-- | Operates similarly to 'group', grouping by the first element
-- of the pair and returning that element in pair with each group.
--
-- >>> groupByFst [(1, "a"), (1, "b"), (2, "c"), (1, "d")]
-- [(1,"a" :| ["b"]),(2,"c" :| []),(1,"d" :| [])]
groupByFst :: Eq a => [(a, b)] -> [(a, NonEmpty b)]
groupByFst = groupByKey id
