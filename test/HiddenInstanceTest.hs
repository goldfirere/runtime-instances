{-# LANGUAGE OverloadedStrings #-}

module HiddenInstanceTest where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad ( join )
import Data.Proxy
import Type.Reflection.Name

import Instance.Runtime
import Class
import Util

tests1 :: Instances Unqualified ReadShowEq -> TestTree
tests1 instances = testGroup "hidden1"
  [ testCase "read" $
      join $
      assertJust "withInstance" $
      withInstanceProxy instances "T" $ \ (_ :: Proxy t) ->
      read @t "MkT 1" @?= read "MkT  0x1"
  , testCase "show" $
      join $
      assertJust "withInstance" $
      withInstanceProxy instances "T" $ \ (_ :: Proxy t) ->
      show (read @t "  MkT  0002") @?= "MkT 2"
  , testCase "read T3" $
      join $
      assertJust "withInstance" $
      withInstanceProxy instances "T3" $ \ (_ :: Proxy t) ->
      read @t "MkT3 3.14" @?= read "MkT3   3.1400"
  ]

tests2 :: Instances Unqualified ReadShowEq2 -> TestTree
tests2 instances = testGroup "hidden2"
  [ testCase "read" $
      join $
      assertJust "withInstance" $
      withInstanceProxy instances "T" $ \ (_ :: Proxy t) ->
      read @t "MkT 1" @?= read "MkT  0x1"
  , testCase "show" $
      join $
      assertJust "withInstance" $
      withInstanceProxy instances "T" $ \ (_ :: Proxy t) ->
      show (read @t "  MkT  0002") @?= "MkT 2"
  , testCase "read T3" $
      join $
      assertJust "withInstance" $
      withInstanceProxy instances "T3" $ \ (_ :: Proxy t) ->
      read @t "MkT3 3.14" @?= read "MkT3   3.1400"
  ]