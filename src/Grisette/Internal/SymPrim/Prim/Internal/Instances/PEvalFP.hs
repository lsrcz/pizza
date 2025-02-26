{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP () where

import qualified Data.SBV as SBV
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPOp
      ( fpMaximum,
        fpMaximumNumber,
        fpMinimum,
        fpMinimumNumber,
        fpRem
      ),
    IEEEFPRoundingOp
      ( fpAdd,
        fpDiv,
        fpFMA,
        fpMul,
        fpRoundToIntegral,
        fpSqrt,
        fpSub
      ),
  )
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPBinaryOp (FPMaximum, FPMaximumNumber, FPMinimum, FPMinimumNumber, FPRem),
    FPRoundingBinaryOp (FPAdd, FPDiv, FPMul, FPSub),
    FPRoundingUnaryOp (FPRoundToIntegral, FPSqrt),
    FPTrait
      ( FPIsInfinite,
        FPIsNaN,
        FPIsNegative,
        FPIsNegativeInfinite,
        FPIsNegativeZero,
        FPIsNormal,
        FPIsPoint,
        FPIsPositive,
        FPIsPositiveInfinite,
        FPIsPositiveZero,
        FPIsSubnormal,
        FPIsZero
      ),
    FPUnaryOp (FPAbs, FPNeg),
    PEvalFPTerm
      ( pevalFPBinaryTerm,
        pevalFPFMATerm,
        pevalFPRoundingBinaryTerm,
        pevalFPRoundingUnaryTerm,
        pevalFPTraitTerm,
        pevalFPUnaryTerm,
        sbvFPBinaryTerm,
        sbvFPFMATerm,
        sbvFPRoundingBinaryTerm,
        sbvFPRoundingUnaryTerm,
        sbvFPTraitTerm,
        sbvFPUnaryTerm
      ),
    conTerm,
    fpBinaryTerm,
    fpFMATerm,
    fpRoundingBinaryTerm,
    fpRoundingUnaryTerm,
    fpTraitTerm,
    fpUnaryTerm,
    pattern ConTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)

instance PEvalFPTerm FP where
  pevalFPTraitTerm trait =
    unaryUnfoldOnce doPevalFPTraitTerm (fpTraitTerm trait)
    where
      doPevalFPTraitTerm (ConTerm a) = case trait of
        FPIsNaN -> Just $ conTerm $ isNaN a
        FPIsPositive ->
          Just $
            conTerm $
              not (isNaN a) && a >= 0 && not (isNegativeZero a)
        FPIsNegative ->
          Just $ conTerm $ not (isNaN a) && (a < 0 || isNegativeZero a)
        FPIsInfinite -> Just $ conTerm $ isInfinite a
        FPIsPositiveInfinite -> Just $ conTerm $ isInfinite a && a > 0
        FPIsNegativeInfinite -> Just $ conTerm $ isInfinite a && a < 0
        FPIsPositiveZero ->
          Just $ conTerm $ a == 0 && not (isNegativeZero a)
        FPIsNegativeZero -> Just $ conTerm $ isNegativeZero a
        FPIsZero -> Just $ conTerm $ a == 0
        FPIsNormal ->
          Just $
            conTerm $
              not (a == 0 || isNaN a || isInfinite a || isDenormalized a)
        FPIsSubnormal -> Just $ conTerm $ isDenormalized a
        FPIsPoint -> Just $ conTerm $ not (isNaN a || isInfinite a)
      doPevalFPTraitTerm _ = Nothing
  pevalFPUnaryTerm = fpUnaryTerm
  {-# INLINE pevalFPUnaryTerm #-}
  pevalFPBinaryTerm bop (ConTerm l) (ConTerm r) =
    case bop of
      FPMaximum -> conTerm $ fpMaximum l r
      FPMaximumNumber -> conTerm $ fpMaximumNumber l r
      FPMinimum -> conTerm $ fpMinimum l r
      FPMinimumNumber -> conTerm $ fpMinimumNumber l r
      FPRem -> conTerm $ fpRem l r
  pevalFPBinaryTerm FPMaximum l r | l == r = l
  pevalFPBinaryTerm FPMaximumNumber l r | l == r = l
  pevalFPBinaryTerm FPMinimum l r | l == r = l
  pevalFPBinaryTerm FPMinimumNumber l r | l == r = l
  pevalFPBinaryTerm bop l r = fpBinaryTerm bop l r
  {-# INLINE pevalFPBinaryTerm #-}
  pevalFPRoundingUnaryTerm uop (ConTerm rd) (ConTerm l) =
    case uop of
      FPSqrt -> conTerm $ fpSqrt rd l
      FPRoundToIntegral -> conTerm $ fpRoundToIntegral rd l
  pevalFPRoundingUnaryTerm uop rd l = fpRoundingUnaryTerm uop rd l
  {-# INLINE pevalFPRoundingUnaryTerm #-}
  pevalFPRoundingBinaryTerm bop (ConTerm rd) (ConTerm l) (ConTerm r) =
    case bop of
      FPAdd -> conTerm $ fpAdd rd l r
      FPSub -> conTerm $ fpSub rd l r
      FPMul -> conTerm $ fpMul rd l r
      FPDiv -> conTerm $ fpDiv rd l r
  pevalFPRoundingBinaryTerm bop rd l r = fpRoundingBinaryTerm bop rd l r
  {-# INLINE pevalFPRoundingBinaryTerm #-}
  pevalFPFMATerm (ConTerm rd) (ConTerm x) (ConTerm y) (ConTerm z) =
    conTerm $ fpFMA rd x y z
  pevalFPFMATerm rd x y z = fpFMATerm rd x y z
  {-# INLINE pevalFPFMATerm #-}

  sbvFPTraitTerm FPIsNaN = SBV.fpIsNaN
  sbvFPTraitTerm FPIsPositive = goodFpIsPositive
  sbvFPTraitTerm FPIsNegative = goodFpIsNegative
  sbvFPTraitTerm FPIsInfinite = SBV.fpIsInfinite
  sbvFPTraitTerm FPIsPositiveInfinite = \f ->
    SBV.fpIsInfinite f SBV..&& goodFpIsPositive f
  sbvFPTraitTerm FPIsNegativeInfinite = \f ->
    SBV.fpIsInfinite f SBV..&& goodFpIsNegative f
  sbvFPTraitTerm FPIsPositiveZero =
    \f -> SBV.fpIsZero f SBV..&& goodFpIsPositive f
  sbvFPTraitTerm FPIsNegativeZero =
    \f -> SBV.fpIsZero f SBV..&& goodFpIsNegative f
  sbvFPTraitTerm FPIsZero = SBV.fpIsZero
  sbvFPTraitTerm FPIsNormal = SBV.fpIsNormal
  sbvFPTraitTerm FPIsSubnormal = SBV.fpIsSubnormal
  sbvFPTraitTerm FPIsPoint = SBV.fpIsPoint

  sbvFPUnaryTerm FPAbs = SBV.fpAbs
  sbvFPUnaryTerm FPNeg = SBV.fpNeg
  {-# INLINE sbvFPUnaryTerm #-}

  sbvFPBinaryTerm FPRem x y = SBV.fpRem x y
  sbvFPBinaryTerm FPMinimum x y =
    SBV.ite (SBV.fpIsNaN x SBV..|| SBV.fpIsNaN y) SBV.nan $
      SBV.ite (sbvCmpHandleNegZero x y) x y
  sbvFPBinaryTerm FPMinimumNumber x y =
    SBV.ite (SBV.fpIsNaN x) y $
      SBV.ite (SBV.fpIsNaN y) x $
        SBV.ite (sbvCmpHandleNegZero x y) x y
  sbvFPBinaryTerm FPMaximum x y =
    SBV.ite (SBV.fpIsNaN x SBV..|| SBV.fpIsNaN y) SBV.nan $
      SBV.ite (sbvCmpHandleNegZero x y) y x
  sbvFPBinaryTerm FPMaximumNumber x y =
    SBV.ite (SBV.fpIsNaN x) y $
      SBV.ite (SBV.fpIsNaN y) x $
        SBV.ite (sbvCmpHandleNegZero x y) y x
  {-# INLINE sbvFPBinaryTerm #-}

  sbvFPRoundingUnaryTerm FPSqrt = SBV.fpSqrt
  sbvFPRoundingUnaryTerm FPRoundToIntegral = SBV.fpRoundToIntegral
  {-# INLINE sbvFPRoundingUnaryTerm #-}

  sbvFPRoundingBinaryTerm FPAdd = SBV.fpAdd
  sbvFPRoundingBinaryTerm FPSub = SBV.fpSub
  sbvFPRoundingBinaryTerm FPMul = SBV.fpMul
  sbvFPRoundingBinaryTerm FPDiv = SBV.fpDiv
  {-# INLINE sbvFPRoundingBinaryTerm #-}

  sbvFPFMATerm = SBV.fpFMA
  {-# INLINE sbvFPFMATerm #-}

goodFpIsPositive :: (ValidFP eb sb) => SBV.SFloatingPoint eb sb -> SBV.SBool
goodFpIsPositive x = SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.fpIsPositive x
{-# INLINE goodFpIsPositive #-}

goodFpIsNegative :: (ValidFP eb sb) => SBV.SFloatingPoint eb sb -> SBV.SBool
goodFpIsNegative x = SBV.sNot (SBV.fpIsNaN x) SBV..&& SBV.fpIsNegative x
{-# INLINE goodFpIsNegative #-}

sbvCmpHandleNegZero ::
  (ValidFP eb sb) =>
  SBV.SFloatingPoint eb sb ->
  SBV.SFloatingPoint eb sb ->
  SBV.SBool
sbvCmpHandleNegZero x y =
  SBV.ite
    (SBV.fpIsZero x SBV..&& SBV.fpIsZero y)
    (SBV.fpIsNegativeZero x)
    (x SBV..< y)
{-# INLINE sbvCmpHandleNegZero #-}
