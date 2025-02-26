{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalDivModIntegralTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalDivModIntegralTerm
  ( pevalDefaultDivIntegralTerm,
    pevalDefaultDivBoundedIntegralTerm,
    pevalDefaultModIntegralTerm,
    pevalDefaultQuotIntegralTerm,
    pevalDefaultQuotBoundedIntegralTerm,
    pevalDefaultRemIntegralTerm,
  )
where

import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm,
        pevalQuotIntegralTerm,
        pevalRemIntegralTerm,
        withSbvDivModIntegralTermConstraint
      ),
    SupportedPrim (withPrim),
    Term,
    conTerm,
    divIntegralTerm,
    modIntegralTerm,
    quotIntegralTerm,
    remIntegralTerm,
    pattern ConTerm,
    pattern SupportedTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

-- | Default partial evaluation of division operation for integral types.
pevalDefaultDivIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultDivIntegralTerm l@SupportedTerm r =
  binaryUnfoldOnce doPevalDefaultDivIntegralTerm divIntegralTerm l r

doPevalDefaultDivIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultDivIntegralTerm (ConTerm a) (ConTerm b)
  | b /= 0 = Just $ conTerm $ a `div` b
doPevalDefaultDivIntegralTerm a (ConTerm 1) = Just a
doPevalDefaultDivIntegralTerm _ _ = Nothing

-- | Default partial evaluation of division operation for bounded integral
-- types.
pevalDefaultDivBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Term a
pevalDefaultDivBoundedIntegralTerm l@SupportedTerm r =
  binaryUnfoldOnce doPevalDefaultDivBoundedIntegralTerm divIntegralTerm l r

doPevalDefaultDivBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalDefaultDivBoundedIntegralTerm (ConTerm a) (ConTerm b)
  | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `div` b
doPevalDefaultDivBoundedIntegralTerm a (ConTerm 1) = Just a
doPevalDefaultDivBoundedIntegralTerm _ _ = Nothing

-- | Default partial evaluation of modulo operation for integral types.
pevalDefaultModIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultModIntegralTerm l@SupportedTerm r =
  binaryUnfoldOnce doPevalDefaultModIntegralTerm modIntegralTerm l r

doPevalDefaultModIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultModIntegralTerm (ConTerm a) (ConTerm b)
  | b /= 0 = Just $ conTerm $ a `mod` b
doPevalDefaultModIntegralTerm _ (ConTerm 1) = Just $ conTerm 0
doPevalDefaultModIntegralTerm _ (ConTerm (-1)) = Just $ conTerm 0
doPevalDefaultModIntegralTerm _ _ = Nothing

-- | Default partial evaluation of quotient operation for integral types.
pevalDefaultQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultQuotIntegralTerm l@SupportedTerm r =
  binaryUnfoldOnce doPevalDefaultQuotIntegralTerm quotIntegralTerm l r

doPevalDefaultQuotIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultQuotIntegralTerm (ConTerm a) (ConTerm b)
  | b /= 0 = Just $ conTerm $ a `quot` b
doPevalDefaultQuotIntegralTerm a (ConTerm 1) = Just a
doPevalDefaultQuotIntegralTerm _ _ = Nothing

-- | Default partial evaluation of quotient operation for bounded integral
-- types.
pevalDefaultQuotBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Term a
pevalDefaultQuotBoundedIntegralTerm l@SupportedTerm r =
  binaryUnfoldOnce doPevalDefaultQuotBoundedIntegralTerm quotIntegralTerm l r

doPevalDefaultQuotBoundedIntegralTerm ::
  (PEvalDivModIntegralTerm a, Bounded a, Integral a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalDefaultQuotBoundedIntegralTerm (ConTerm a) (ConTerm b)
  | b /= 0 && (b /= -1 || a /= minBound) = Just $ conTerm $ a `quot` b
doPevalDefaultQuotBoundedIntegralTerm a (ConTerm 1) = Just a
doPevalDefaultQuotBoundedIntegralTerm _ _ = Nothing

-- | Default partial evaluation of remainder operation for integral types.
pevalDefaultRemIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Term a
pevalDefaultRemIntegralTerm l@SupportedTerm r =
  binaryUnfoldOnce doPevalDefaultRemIntegralTerm remIntegralTerm l r

doPevalDefaultRemIntegralTerm ::
  (PEvalDivModIntegralTerm a, Integral a) => Term a -> Term a -> Maybe (Term a)
doPevalDefaultRemIntegralTerm (ConTerm a) (ConTerm b)
  | b /= 0 = Just $ conTerm $ a `rem` b
doPevalDefaultRemIntegralTerm _ (ConTerm 1) = Just $ conTerm 0
doPevalDefaultRemIntegralTerm _ (ConTerm (-1)) = Just $ conTerm 0
doPevalDefaultRemIntegralTerm _ _ = Nothing

instance PEvalDivModIntegralTerm Integer where
  pevalDivIntegralTerm = pevalDefaultDivIntegralTerm
  pevalModIntegralTerm = pevalDefaultModIntegralTerm
  pevalQuotIntegralTerm = pevalDefaultQuotIntegralTerm
  pevalRemIntegralTerm = pevalDefaultRemIntegralTerm
  withSbvDivModIntegralTermConstraint r = r

instance (KnownNat n, 1 <= n) => PEvalDivModIntegralTerm (IntN n) where
  pevalDivIntegralTerm = pevalDefaultDivBoundedIntegralTerm
  pevalModIntegralTerm = pevalDefaultModIntegralTerm
  pevalQuotIntegralTerm = pevalDefaultQuotBoundedIntegralTerm
  pevalRemIntegralTerm = pevalDefaultRemIntegralTerm
  withSbvDivModIntegralTermConstraint r = withPrim @(IntN n) r

instance (KnownNat n, 1 <= n) => PEvalDivModIntegralTerm (WordN n) where
  pevalDivIntegralTerm = pevalDefaultDivIntegralTerm
  pevalModIntegralTerm = pevalDefaultModIntegralTerm
  pevalQuotIntegralTerm = pevalDefaultQuotIntegralTerm
  pevalRemIntegralTerm = pevalDefaultRemIntegralTerm
  withSbvDivModIntegralTermConstraint r = withPrim @(WordN n) r
