module Main
  ( main
  ) where

import Test.Tasty
import Tree (tests)

main :: IO ()
main = tests >>= defaultMain
