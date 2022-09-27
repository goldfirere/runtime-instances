{-# LANGUAGE AllowAmbiguousTypes, DataKinds, GADTs, QuantifiedConstraints, CPP #-}

{-|
Description : Find a class instance at runtime
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

The key innovation in this library is the 'Instances' type, which is a database
of instances that can be queried at runtime. More specifically, an @'Instances' tt c@
is a mapping from elements of type @tt@ to instances of class @c@.

It is expected
that @tt@ be an instance of the 'TypeText' class, which controls how types are rendered
into text. A good initial choice for @tt@ is 'Unqualified', which will render all types
as unqualified names. This is simple, but potentially ambiguous, if you have multiple
types with the same name (declared in different modules). See "Type.Reflection.Name"
for other alternatives.

An important restriction is that 'Instances' can hold only /ground/ instances: instances
with no type variables or type families. Maybe we can accommodate non-ground instances
in the future, but not today. (There seems to be no fundamental reason we cannot support
non-ground instances, but doing so would be a good deal harder than ground instances.)
The instance declarations may have variables, even though
the 'Instances' database will hold those instances' instantiations. For example, while
we have @instance Eq a => Eq (Maybe a)@, that polymorphic instance cannot be stored in
'Instances'. Instead, you can build an @'Instances' 'Unqualified' 'Eq'@
with, say, @Eq (Maybe Int)@, @Eq (Maybe Bool)@, and @Eq (Maybe Double)@. Looking up
@Maybe Int@, @Maybe Bool@, and @Maybe Double@ will all succeed, but looking up @Maybe Char@
would fail.

To create an `Instances`, use the @instancesFor...@ functions. The @Invisible@ variants
accept a type argument specifying the (ground) types which should be used to populate the
`Instances` database. The @TypeRep@ variant expects a 'TypeRep'. In the future, we expect
to offer a @instancesFor@ function which will use [visible dependent quantification](https://github.com/ghc-proposals/ghc-proposals/pull/281) to accept the list of types. Note that 'Instances'
is a 'Monoid', so you can combine the results of several calls.

In order to build an 'Instances' containing all instances in scope, see "Instance.Runtime.TH".

To use an 'Instances', use the 'withInstanceProxy' function. This function looks up an instance
in the database and, if successful, passes that instance to a callback function. In the future,
once we have the ability to [bind type variables in a lambda](https://github.com/ghc-proposals/ghc-proposals/pull/448), we expect 'withInstance' to be a better interface.
-}

module Instance.Runtime (
  Instances,

  -- ** Creation

  instanceForTypeRep, instanceForInvisible,
  instancesForInvisible,

  -- ** Usage

  withInstance, withInstanceProxy, withInstanceTypeRep,

  -- ** Folding

  foldInstances,
  ) where

import qualified Data.Map.Lazy as M
import Data.SOP.NP
import Data.SOP.Constraint ( All )

import Type.Reflection.List
import Type.Reflection.Name

import Data.Proxy
import Data.Kind
import Type.Reflection
import Data.Functor

#if __GLASGOW_HASKELL__ < 904
import Unsafe.Coerce
#endif

-- | A database of instances available for runtime queries. An @Instances tn c@ contains
-- instances of the class @c@, indexed by type names rendered according to the rules for @tn@.
type Instances :: forall k. Type -> (k -> Constraint) -> Type
newtype Instances type_namer c = MkInstances (M.Map type_namer (EDict c))
  deriving (Semigroup, Monoid)

-- | for debugging only
instance (Show type_namer, Typeable c) => Show (Instances type_namer c) where
  show (MkInstances m) = "(Instances for " ++ show (typeRep @c) ++ " for types " ++ show (M.keys m) ++ ")"

type EDict :: forall k. (k -> Constraint) -> Type
data EDict c where
  PackDict :: forall x c. c x => EDict c

-- | Create an 'Instances' for a type denoted by the given 'TypeRep'.
instanceForTypeRep ::
  forall {k} (x :: k) (c :: k -> Constraint) tn.
  c x =>
  TypeText tn =>
  TypeRep x ->
  Instances tn c
instanceForTypeRep tr = MkInstances (M.singleton (renderTypeRep tr) (PackDict @x))

-- | Create an 'Instances' for a type passed invisibly. Example: @instanceForInvisible @Int@.
instanceForInvisible ::
  forall {k} (x :: k) (c :: k -> Constraint) tn.
  Typeable x =>
  c x =>
  TypeText tn =>
  Instances tn c
instanceForInvisible = instanceForTypeRep (typeRep @x)

-- | Create an 'Instances' for a list of types passed invisibly.
-- Example: @instancesForInvisible @[Int, Bool, Double]@.
instancesForInvisible ::
  forall {k} (xs :: [k]) (c :: k -> Constraint) tn.
  Typeable xs =>
  All c xs =>
  TypeText tn =>
  Instances tn c
instancesForInvisible = go all_trs
  where
    tr_xs   = typeRep @xs
    all_trs = typeRepList tr_xs

    go :: forall (inner_xs :: [k]). All c inner_xs => NP TypeRep inner_xs -> Instances tn c
    go (tr :* trs) = instanceForTypeRep tr <> go trs
    go Nil         = mempty

-- | Look up an instance in 'Instances', making the instance available for use in the continuation
-- function. If the lookup fails, this returns Nothing. Until https://github.com/ghc-proposals/ghc-proposals/pull/448
-- is implemented, there is no way to bind a type variable to the found type, so this function is likely
-- impossible to use well. For now, see 'withInstanceProxy' instead.
withInstance :: Ord tn => Instances tn c -> tn -> (forall x. c x => r) -> Maybe r
withInstance inst_db type_name f = withInstanceProxy inst_db type_name (\ (_ :: Proxy x) -> f @x)

-- | Look up an instance in 'Instances', making the instance available for use in the continuation
-- function. If the lookup fails, this returns Nothing. If https://github.com/ghc-proposals/ghc-proposals/pull/448
-- has been implemented in your GHC, you may prefer 'withInstance'.
withInstanceProxy :: Ord tn => Instances tn c -> tn -> (forall x. c x => Proxy x -> r) -> Maybe r
withInstanceProxy (MkInstances mapping) type_name f =
  M.lookup type_name mapping <&> \ (PackDict @_ @x) -> f (Proxy @x)
    -- The @_ in PackDict above is a GHC bug (GitLab is wonky at the moment, so I can't search for
    -- ticket #, but I'm pretty sure I remember this one and that it's fixed in HEAD)

-- | Look up an instance in 'Instances', when you already have a 'TypeRep' for the type to look up.
-- To use this function, the class @c@ used in your @Instances@ must imply 'Typeable'; the 'Typeable'
-- instance is used to check that the retrieved type is actually the one you want. If it's not,
-- this function throws an exception (this really shouldn't happen).
-- (In GHCs before 9.4, this check is skipped, because of a bug around 'Typeable' and quantified
-- constraints.)
-- If the lookup fails, this returns Nothing.
withInstanceTypeRep ::
  forall t c tn r.
  TypeText tn =>
#if __GLASGOW_HASKELL__ >= 904
  (forall x. c x => Typeable x) =>
#endif
  Instances tn c -> TypeRep t -> (c t => r) -> Maybe r
withInstanceTypeRep instances type_rep action =
  withInstanceProxy instances (renderTypeRep type_rep) $ \ (_ :: Proxy t') ->
    let
        type_rep' :: TypeRep t'
#if __GLASGOW_HASKELL__ >= 904
        type_rep' = typeRep
#else
        type_rep' = unsafeCoerce type_rep
#endif
    in
    case type_rep `eqTypeRep` type_rep' of
      Just HRefl -> action
      Nothing -> error $ "Retrieved type " ++ show type_rep' ++
                         " different from expected type " ++ show type_rep ++ "."

-- | Perform a computation for each instance in the database, accumulating results
-- in a monoid. Your monoid should be commutative, because there is no predictable
-- order of instances in the 'Instances'.
foldInstances ::
  Monoid m =>
  Instances tn c ->
  (forall x. c x => Proxy x -> m) ->
  m
foldInstances (MkInstances mapping) action =
  foldMap (\ (PackDict @_ @x) -> action @x Proxy) mapping