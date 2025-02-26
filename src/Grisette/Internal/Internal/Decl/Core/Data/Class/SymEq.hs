{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( -- * Symbolic equality
    SymEq (..),
    SymEq1 (..),
    symEq1,
    SymEq2 (..),
    symEq2,
    pairwiseSymDistinct,

    -- * More 'Eq' helper
    distinct,

    -- * Generic 'SymEq'
    SymEqArgs (..),
    GSymEq (..),
    genericSymEq,
    genericLiftSymEq,
  )
where

import Data.Kind (Type)
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    Generic (Rep, from),
    Generic1 (Rep1, from1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&)))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- | Check if all elements in a list are distinct.
--
-- Note that empty or singleton lists are always distinct.
--
-- >>> distinct []
-- True
-- >>> distinct [1]
-- True
-- >>> distinct [1, 2, 3]
-- True
-- >>> distinct [1, 2, 2]
-- False
distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct [_] = True
distinct (x : xs) = go x xs && distinct xs
  where
    go _ [] = True
    go x' (y : ys) = x' /= y .&& go x' ys

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Symbolic equality. Note that we can't use Haskell's 'Eq' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a .== b
-- false
-- >>> a ./= b
-- true
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a .== b
-- (= a b)
-- >>> a ./= b
-- (! (= a b))
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SymEq via (Default X)
class SymEq a where
  (.==) :: a -> a -> SymBool
  a .== b = symNot $ a ./= b
  {-# INLINE (.==) #-}
  infix 4 .==

  (./=) :: a -> a -> SymBool
  a ./= b = symNot $ a .== b
  {-# INLINE (./=) #-}
  infix 4 ./=

  -- | Check if all elements in a list are distinct, under the symbolic equality
  -- semantics.
  symDistinct :: [a] -> SymBool
  symDistinct = pairwiseSymDistinct

  {-# MINIMAL (.==) | (./=) #-}

-- | Default pairwise symbolic distinct implementation.
pairwiseSymDistinct :: (SymEq a) => [a] -> SymBool
pairwiseSymDistinct [] = con True
pairwiseSymDistinct [_] = con True
pairwiseSymDistinct (x : xs) = go x xs .&& pairwiseSymDistinct xs
  where
    go _ [] = con True
    go x' (y : ys) = x' ./= y .&& go x' ys

-- | Lifting of the 'SymEq' class to unary type constructors.
--
-- Any instance should be subject to the following law that canonicity is
-- preserved:
--
-- @liftSymEq (.==)@ should be equivalent to @(.==)@, under the symbolic
-- semantics.
--
-- This class therefore represents the generalization of 'SymEq' by decomposing
-- its main method into a canonical lifting on a canonical inner method, so that
-- the lifting can be reused for other arguments than the canonical one.
class (forall a. (SymEq a) => SymEq (f a)) => SymEq1 f where
  -- | Lift a symbolic equality test through the type constructor.
  --
  -- The function will usually be applied to an symbolic equality function, but
  -- the more general type ensures that the implementation uses it to compare
  -- elements of the first container with elements of the second.
  liftSymEq :: (a -> b -> SymBool) -> f a -> f b -> SymBool

-- | Lift the standard @('.==')@ function through the type constructor.
symEq1 :: (SymEq a, SymEq1 f) => f a -> f a -> SymBool
symEq1 = liftSymEq (.==)

-- | Lifting of the 'SymEq' class to binary type constructors.
class (forall a. (SymEq a) => SymEq1 (f a)) => SymEq2 f where
  -- | Lift symbolic equality tests through the type constructor.
  --
  -- The function will usually be applied to an symbolic equality function, but
  -- the more general type ensures that the implementation uses it to compare
  -- elements of the first container with elements of the second.
  liftSymEq2 ::
    (a -> b -> SymBool) ->
    (c -> d -> SymBool) ->
    f a c ->
    f b d ->
    SymBool

-- | Lift the standard @('.==')@ function through the type constructor.
symEq2 :: (SymEq a, SymEq b, SymEq2 f) => f a b -> f a b -> SymBool
symEq2 = liftSymEq2 (.==) (.==)

-- Derivations

-- | The arguments to the generic equality function.
data family SymEqArgs arity a b :: Type

data instance SymEqArgs Arity0 _ _ = SymEqArgs0

newtype instance SymEqArgs Arity1 a b = SymEqArgs1 (a -> b -> SymBool)

-- | The class of types that can be generically compared for symbolic equality.
class GSymEq arity f where
  gsymEq :: SymEqArgs arity a b -> f a -> f b -> SymBool

instance GSymEq arity V1 where
  gsymEq _ _ _ = con True
  {-# INLINE gsymEq #-}

instance GSymEq arity U1 where
  gsymEq _ _ _ = con True
  {-# INLINE gsymEq #-}

instance (GSymEq arity a, GSymEq arity b) => GSymEq arity (a :*: b) where
  gsymEq args (a1 :*: b1) (a2 :*: b2) = gsymEq args a1 a2 .&& gsymEq args b1 b2
  {-# INLINE gsymEq #-}

instance (GSymEq arity a, GSymEq arity b) => GSymEq arity (a :+: b) where
  gsymEq args (L1 a1) (L1 a2) = gsymEq args a1 a2
  gsymEq args (R1 b1) (R1 b2) = gsymEq args b1 b2
  gsymEq _ _ _ = con False
  {-# INLINE gsymEq #-}

instance (GSymEq arity a) => GSymEq arity (M1 i c a) where
  gsymEq args (M1 a1) (M1 a2) = gsymEq args a1 a2
  {-# INLINE gsymEq #-}

instance (SymEq a) => GSymEq arity (K1 i a) where
  gsymEq _ (K1 a) (K1 b) = a .== b
  {-# INLINE gsymEq #-}

instance GSymEq Arity1 Par1 where
  gsymEq (SymEqArgs1 e) (Par1 a) (Par1 b) = e a b
  {-# INLINE gsymEq #-}

instance (SymEq1 f) => GSymEq Arity1 (Rec1 f) where
  gsymEq (SymEqArgs1 e) (Rec1 a) (Rec1 b) = liftSymEq e a b
  {-# INLINE gsymEq #-}

instance (SymEq1 f, GSymEq Arity1 g) => GSymEq Arity1 (f :.: g) where
  gsymEq targs (Comp1 a) (Comp1 b) = liftSymEq (gsymEq targs) a b
  {-# INLINE gsymEq #-}

instance (Generic a, GSymEq Arity0 (Rep a)) => SymEq (Default a) where
  Default l .== Default r = genericSymEq l r
  {-# INLINE (.==) #-}

-- | Generic @('.==')@ function.
genericSymEq :: (Generic a, GSymEq Arity0 (Rep a)) => a -> a -> SymBool
genericSymEq l r = gsymEq SymEqArgs0 (from l) (from r)
{-# INLINE genericSymEq #-}

instance (Generic1 f, GSymEq Arity1 (Rep1 f), SymEq a) => SymEq (Default1 f a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (Generic1 f, GSymEq Arity1 (Rep1 f)) => SymEq1 (Default1 f) where
  liftSymEq f (Default1 l) (Default1 r) = genericLiftSymEq f l r
  {-# INLINE liftSymEq #-}

-- | Generic 'liftSymEq' function.
genericLiftSymEq ::
  (Generic1 f, GSymEq Arity1 (Rep1 f)) =>
  (a -> b -> SymBool) ->
  f a ->
  f b ->
  SymBool
genericLiftSymEq f l r = gsymEq (SymEqArgs1 f) (from1 l) (from1 r)
{-# INLINE genericLiftSymEq #-}
