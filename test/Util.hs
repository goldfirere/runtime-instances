module Util (
  assertJust,
  ) where

import Test.Tasty.HUnit

assertJust :: String -> Maybe a -> IO a
assertJust err Nothing  = assertFailure (err ++ ": expected Just but got Nothing")
assertJust _   (Just x) = return x