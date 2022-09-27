module Main where

import Test.Tasty

import qualified HiddenInstances
import qualified HiddenInstanceTest

main :: IO ()
main = defaultMain all_tests

all_tests :: TestTree
all_tests = testGroup "All"
  [ HiddenInstanceTest.tests1 HiddenInstances.t_instances
  , HiddenInstanceTest.tests2 HiddenInstances.all_t_instances
  ]
