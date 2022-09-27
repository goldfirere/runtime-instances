{-# LANGUAGE UndecidableInstances #-}

module Class (
  ReadShowEq, ReadShowEq2,
  ) where

class (Read a, Show a, Eq a) => ReadShowEq a
instance (Read a, Show a, Eq a) => ReadShowEq a

-- this one has concrete instances, for allGroundInstances
class (Read a, Show a, Eq a) => ReadShowEq2 a