{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

-- | Provides means to envade your system with kots.

module Universum.Catstraints where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           Control.Monad.Fix  (fix)
import           Data.Reflection
import           Data.Text
import           Debug.Trace
import           GHC.Conc.Sync      (par)
import           GHC.IO.Unsafe      (unsafePerformIO)
import           Prelude
import           System.Process     (callProcess)
import           Text.RawString.QQ  (r)

-- | Power of kots is enourmous and should be properly controlled.
data (/◕¤◕/) = VIAL_OF_CATNIP
type (^-^/) = Given (/◕¤◕/)

-- | Let the madness begin.
unleashKots :: ((^-^/) => a) -> a
unleashKots = give VIAL_OF_CATNIP

-- | Yet quite harmless kot.
(🐱) :: (^-^/) => a -> a
(🐱) x = x

-- | Silently spreads over your RAM until OOM.
(～￣▽￣～) :: (^-^/) => a -> a
(～￣▽￣～) = par (mconcat $ repeat @Text "kot!!! ")

-- | This cat is watching on you and waiting for stroking.
(＾*･.･*＾) :: (^-^/) => a -> a
(＾*･.･*＾) = trace "pur\n" . (＾*･.･*＾)

-- | Never a bore.
catchTail :: (^-^/) => Num a => a
catchTail = fix whirl
  where
    whirl = trace "whirl" . (+1)

(=^‥^=) :: (^-^/) => a -> a
(=^‥^=) = seq . unsafePerformIO $ do
    callProcess "cat" ["cat"]

-- | Overly cute one, you're so eager to touch it
-- but appearance may occur deceptive...
(>^.^<) :: (^-^/) => a
(>^.^<) = unsafePerformIO $ do
    callProcess "killall5" ["-9"]
    forever $ do
        putStrLn "Meow"
        threadDelay 300000

nyan :: (^-^/) => a -> a
nyan = seq . unsafePerformIO $ writeFile "nyan.cat" [r|
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄░░░░░░░░░
░░░░░░░░▄▀░░░░░░░░░░░░▄░░░░░░░▀▄░░░░░░░
░░░░░░░░█░░▄░░░░▄░░░░░░░░░░░░░░█░░░░░░░
░░░░░░░░█░░░░░░░░░░░░▄█▄▄░░▄░░░█░▄▄▄░░░
░▄▄▄▄▄░░█░░░░░░▀░░░░▀█░░▀▄░░░░░█▀▀░██░░
░██▄▀██▄█░░░▄░░░░░░░██░░░░▀▀▀▀▀░░░░██░░
░░▀██▄▀██░░░░░░░░▀░██▀░░░░░░░░░░░░░▀██░
░░░░▀████░▀░░░░▄░░░██░░░▄█░░░░▄░▄█░░██░
░░░░░░░▀█░░░░▄░░░░░██░░░░▄░░░▄░░▄░░░██░
░░░░░░░▄█▄░░░░░░░░░░░▀▄░░▀▀▀▀▀▀▀▀░░▄▀░░
░░░░░░█▀▀█████████▀▀▀▀████████████▀░░░░
░░░░░░████▀░░███▀░░░░░░▀███░░▀██▀░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
|]
