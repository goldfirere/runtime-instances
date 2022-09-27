{-# LANGUAGE DataKinds, TemplateHaskell, DeriveAnyClass #-}

module HiddenInstances (
  t_instances, all_t_instances,
  ) where

import Type.Reflection.Name

import Instance.Runtime
import Instance.Runtime.TH
import Class

data T = MkT Int
  deriving (Read, Show, Eq, ReadShowEq2)

t_instances :: Instances Unqualified ReadShowEq
t_instances = instancesForInvisible @'[T, T2, T3]

data T2 = MkT2 Bool
  deriving (Read, Show, Eq, ReadShowEq2)

data T3 = MkT3 Double
  deriving (Read, Show, Eq, ReadShowEq2)

return []

all_t_instances :: Instances Unqualified ReadShowEq2
all_t_instances = $(allGroundInstances [t| ReadShowEq2 |])