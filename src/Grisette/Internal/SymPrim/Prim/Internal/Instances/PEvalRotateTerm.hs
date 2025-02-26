{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalRotateTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalRotateTerm
  ( pevalFiniteBitsSymRotateRotateLeftTerm,
    pevalFiniteBitsSymRotateRotateRightTerm,
  )
where

import Data.Bits (Bits (rotateR), FiniteBits (finiteBitSize))
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate (symRotate))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm,
        sbvRotateLeftTerm,
        sbvRotateRightTerm,
        withSbvRotateTermConstraint
      ),
    SupportedNonFuncPrim (withNonFuncPrim),
    Term,
    bvIsNonZeroFromGEq1,
    conTerm,
    rotateLeftTerm,
    rotateRightTerm,
    pattern ConTerm,
    pattern SupportedTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)

-- | Partial evaluation of symbolic rotate left term for finite bits types.
pevalFiniteBitsSymRotateRotateLeftTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Term a
pevalFiniteBitsSymRotateRotateLeftTerm t@SupportedTerm n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymRotateRotateLeftTerm` n)
    (`rotateLeftTerm` n)
    t

doPevalFiniteBitsSymRotateRotateLeftTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalFiniteBitsSymRotateRotateLeftTerm (ConTerm a) (ConTerm n)
  | n >= 0 = Just $ conTerm $ symRotate a n -- Just $ conTerm $ rotateL a (fromIntegral n)
doPevalFiniteBitsSymRotateRotateLeftTerm x (ConTerm 0) = Just x
-- doPevalFiniteBitsSymRotateRotateLeftTerm (RotateLeftTerm _ x (ConTerm  n)) (ConTerm  n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalFiniteBitsSymRotateRotateLeftTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymRotateRotateLeftTerm x (ConTerm n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral bs =
      Just $
        pevalFiniteBitsSymRotateRotateLeftTerm
          x
          (conTerm $ n `mod` fromIntegral bs)
  where
    bs = finiteBitSize n
doPevalFiniteBitsSymRotateRotateLeftTerm _ _ = Nothing

-- | Partial evaluation of symbolic rotate right term for finite bits types.
pevalFiniteBitsSymRotateRotateRightTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Term a
pevalFiniteBitsSymRotateRotateRightTerm t@SupportedTerm n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymRotateRotateRightTerm` n)
    (`rotateRightTerm` n)
    t

doPevalFiniteBitsSymRotateRotateRightTerm ::
  forall a.
  (Integral a, SymRotate a, FiniteBits a, PEvalRotateTerm a) =>
  Term a ->
  Term a ->
  Maybe (Term a)
doPevalFiniteBitsSymRotateRotateRightTerm (ConTerm a) (ConTerm n)
  | n >= 0 =
      Just . conTerm $
        rotateR
          a
          ( fromIntegral $
              (fromIntegral n :: Integer)
                `mod` fromIntegral (finiteBitSize n)
          )
doPevalFiniteBitsSymRotateRotateRightTerm x (ConTerm 0) = Just x
-- doPevalFiniteBitsSymRotateRotateRightTerm (RotateRightTerm _ x (ConTerm  n)) (ConTerm  n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalFiniteBitsSymRotateRotateRightTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymRotateRotateRightTerm x (ConTerm n)
  | n >= 0 && (fromIntegral n :: Integer) >= fromIntegral bs =
      Just $
        pevalFiniteBitsSymRotateRotateRightTerm
          x
          (conTerm $ n `mod` fromIntegral bs)
  where
    bs = finiteBitSize n
doPevalFiniteBitsSymRotateRotateRightTerm _ _ = Nothing

instance (KnownNat n, 1 <= n) => PEvalRotateTerm (IntN n) where
  pevalRotateLeftTerm = pevalFiniteBitsSymRotateRotateLeftTerm
  pevalRotateRightTerm = pevalFiniteBitsSymRotateRotateRightTerm
  withSbvRotateTermConstraint r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(IntN n) r

  -- SBV's rotateLeft and rotateRight are broken for signed values, so we have to
  -- do this
  -- https://github.com/LeventErkok/sbv/issues/673
  sbvRotateLeftTerm l r =
    withNonFuncPrim @(IntN n) $
      withSbvRotateTermConstraint @(IntN n) $
        SBV.sFromIntegral $
          SBV.sRotateLeft
            (SBV.sFromIntegral l :: SBV.SWord n)
            (SBV.sFromIntegral r :: SBV.SWord n)
  sbvRotateRightTerm l r =
    withNonFuncPrim @(IntN n) $
      withSbvRotateTermConstraint @(IntN n) $
        SBV.sFromIntegral $
          SBV.sRotateRight
            (SBV.sFromIntegral l :: SBV.SWord n)
            (SBV.sFromIntegral r :: SBV.SWord n)

instance (KnownNat n, 1 <= n) => PEvalRotateTerm (WordN n) where
  pevalRotateLeftTerm = pevalFiniteBitsSymRotateRotateLeftTerm
  pevalRotateRightTerm = pevalFiniteBitsSymRotateRotateRightTerm
  withSbvRotateTermConstraint r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(WordN n) r
