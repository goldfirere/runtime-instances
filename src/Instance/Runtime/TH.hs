{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Description : Build a runtime-instance database of all instances in scope
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

TODO: write description

-}

module Instance.Runtime.TH (
  allGroundInstances, allGroundInstanceTypes,

  -- * Utilities
  promotedList,
  ) where

import Instance.Runtime

import TH.Utilities
import Language.Haskell.TH
import Data.List ( uncons )

-- | Build an 'Instances' containing all in-scope ground instances for the given
-- class. A /ground instance/ is one that includes no type variables. Example
-- usage:
--
-- > instanceDatabase :: Instances Unqualified MyClass
-- > instanceDatabase = $(allGroundInstances [t| MyClass |])
--
-- It is an infelicity in the design of Template Haskell that requires repeating
-- the @MyClass@ part; it should be inferrable.
--
-- Note that this just looks at instance declarations to determine whether an
-- instance is ground. So it would not pick up, e.g. @Eq (Maybe Int)@, because
-- the instance declaration looks like @Eq (Maybe a)@.
--
-- Due to a limitation of Template Haskell, this will find only instances declared
-- in other modules or before a declaration splice in the current module.
-- If you want to find instances declared in the current module, you can add a line
--
-- > $(pure [])
--
-- above the use of 'allGroundInstances' in the file. This line forces GHC to finish
-- processing everything above the line before looking at anything below the line,
-- so those instances declared above the line are available below it.
allGroundInstances :: Q Type   -- ^ The class whose instances to include.
                               -- This type must have the kind @k -> Constraint@ for some @k@
                               -- and include no variables.
                   -> Q Exp
allGroundInstances q_constraint = do
  constraint <- q_constraint
  ground_instance_types <- allGroundInstanceTypes constraint
  let ty_list = foldr (\ h t -> PromotedConsT `AppT` h `AppT` t) PromotedNilT ground_instance_types
  return (VarE 'instancesForInvisible `AppTypeE` ty_list)

-- | Returns a list of ground (= no variables) types that satisfy the given constraint.
-- The passed-in 'Type' must have kind @k -> Constraint@ for some @k@; all the returned
-- types will then have kind @k@.
--
-- This finds only types that appear in ground instances. So if you look for @Eq@, you'll
-- get @Int@, and @Double@, but not @Maybe Int@, even though @Maybe Int@ is a ground type:
-- it comes from @instance ... => Eq (Maybe a)@, which is not a ground instance.
--
-- See also 'allGroundInstances', for more usage information.
allGroundInstanceTypes :: Type -> Q [Type]
allGroundInstanceTypes constraint = do
  (class_name, ct_args) <- case typeToNamedCon constraint of
    Nothing -> fail (show (ppr constraint) ++ " is not headed by a class.")
    Just (nm, args) -> return (nm, args)
  mapM_ checkForVariables ct_args

  instances <- reifyInstances class_name (ct_args ++ [VarT (mkName "a")])
  return [ ty
         | InstanceD _ _ instance_ty _ <- instances
         , Just (_, args) <- pure (typeToNamedCon instance_ty)
         , Just (ty, _) <- pure (uncons (reverse args))
         , hasNoVariables ty
         ]

-- | Issues an error if the type provided has any variables. Never fails.
checkForVariables :: Type -> Q ()
checkForVariables ty
  | hasNoVariables ty = return ()
  | otherwise         = reportError ("`" ++ show (ppr ty) ++ "' has variables; this is not allowed.")

-- | Checks whether a 'Type' has no variables.
hasNoVariables :: Type -> Bool
hasNoVariables (ForallT {}) = False
hasNoVariables (ForallVisT {}) = False
hasNoVariables (AppT ty1 ty2) = hasNoVariables ty1 && hasNoVariables ty2
hasNoVariables (AppKindT ty1 ki2) = hasNoVariables ty1 && hasNoVariables ki2
hasNoVariables (SigT ty ki) = hasNoVariables ty && hasNoVariables ki
hasNoVariables (VarT {}) = False
hasNoVariables (ConT {}) = True
hasNoVariables (PromotedT {}) = True
hasNoVariables (InfixT ty1 _ ty2) = hasNoVariables ty1 && hasNoVariables ty2
hasNoVariables (UInfixT ty1 _ ty2) = hasNoVariables ty1 && hasNoVariables ty2
hasNoVariables (ParensT ty) = hasNoVariables ty
hasNoVariables (TupleT {}) = True
hasNoVariables (UnboxedTupleT {}) = True
hasNoVariables (UnboxedSumT {}) = True
hasNoVariables ArrowT = True
hasNoVariables MulArrowT = True
hasNoVariables EqualityT = True
hasNoVariables ListT = True
hasNoVariables (PromotedTupleT {}) = True
hasNoVariables PromotedConsT = True
hasNoVariables PromotedNilT = True
hasNoVariables StarT = True
hasNoVariables ConstraintT = True
hasNoVariables (LitT {}) = True
hasNoVariables WildCardT = True
hasNoVariables (ImplicitParamT _ ty) = hasNoVariables ty

------------------------------------
-- Utilities

-- | Constructs a promoted list type from a list of types. Useful for
-- synthesizing calls to 'instancesForInvisible' using Template Haskell.
promotedList :: [Type] -> Type
promotedList = foldr (\h t -> PromotedConsT `AppT` h `AppT` t) PromotedNilT