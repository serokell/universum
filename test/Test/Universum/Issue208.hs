{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , UndecidableInstances
  , FlexibleContexts
  , GADTs
  , DerivingStrategies
  , CPP
#-}
module Test.Universum.Issue208
  () where

#if __GLASGOW_HASKELL__ >= 900

import Universum (Container)

-- In ghc-8.6.3 this code will produce a @redundant constraint@ warning.
-- In ghc-9.0.2 and newer no warnings would be produced.
-- Issue #208: https://github.com/serokell/universum/issues/208

newtype Test = Test [Int]
    deriving newtype (Container)

#endif
