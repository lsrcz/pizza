{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalIEEEFPConvertibleTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalIEEEFPConvertibleTerm
  ( genericFPCast,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants
      ( fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPConvertible (fromFPOr, toFP),
    fpIsInfinite,
    fpIsNaN,
    fpIsNegativeInfinite,
    fpIsNegativeZero,
    fpIsPositiveInfinite,
    fpIsPositiveZero,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( ConvertibleBound (convertibleLowerBound, convertibleUpperBound),
    FP,
    FPRoundingMode (RNA, RNE, RTN, RTP, RTZ),
    ValidFP,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalIEEEFPConvertibleTerm
      ( pevalFromFPOrTerm,
        pevalToFPTerm,
        sbvFromFPOrTerm,
        sbvToFPTerm
      ),
    SBVRep (SBVType),
    SupportedPrim (conSBVTerm),
    Term,
    bvIsNonZeroFromGEq1,
    conTerm,
    fromFPOrTerm,
    toFPTerm,
    pattern ConTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (binaryUnfoldOnce)

-- | Generic implementation for floating-point casting operators for sbv.
genericFPCast ::
  forall a r.
  (SBV.HasKind a, SBV.HasKind r) =>
  SBV.SRoundingMode ->
  SBV.SBV a ->
  SBV.SBV r
genericFPCast rm f = SBVI.SBV (SBVI.SVal kTo (Right (SBVI.cache r)))
  where
    kFrom = SBVI.kindOf f
    kTo = SBVI.kindOf (Proxy @r)
    r st = do
      msv <- SBVI.sbvToSV st rm
      xsv <- SBVI.sbvToSV st f
      SBVI.newExpr st kTo $
        SBVI.SBVApp
          (SBVI.IEEEFP (SBVI.FP_Cast kFrom kTo msv))
          [xsv]

boundedSBVFromFPTerm ::
  forall bv (n :: Nat) eb sb r.
  ( ValidFP eb sb,
    SBVI.HasKind r,
    SBVI.SymVal r,
    Bounded r,
    Num r,
    Ord r,
    SBVI.SBV r ~ SBVType (bv n),
    KnownNat n,
    1 <= n,
    ConvertibleBound bv
  ) =>
  SBVI.SBV r ->
  SBVType FPRoundingMode ->
  SBVType (FP eb sb) ->
  SBVI.SBV r
boundedSBVFromFPTerm d mode l =
  SBV.ite
    ( SBV.fpIsNaN l
        SBV..|| SBV.fpIsInfinite l
        SBV..|| l SBV..< bound convertibleLowerBound
        SBV..|| l SBV..> bound convertibleUpperBound
    )
    d
    $ genericFPCast mode l
  where
    lst =
      [ (SBV.sRTP, RTP),
        (SBV.sRTN, RTN),
        (SBV.sRTZ, RTZ),
        (SBV.sRNE, RNE),
        (SBV.sRNA, RNA)
      ]
    bound f =
      foldl
        ( \acc (srm, rm) ->
            SBV.ite
              (mode SBV..== srm)
              (conSBVTerm (f (undefined :: bv n) rm :: FP eb sb))
              acc
        )
        (0 :: SBVType (FP eb sb))
        lst

generalPevalFromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    SupportedPrim a,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term a ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term a
generalPevalFromFPOrTerm (ConTerm d) (ConTerm rd) (ConTerm f) =
  conTerm $ fromFPOr d rd f
generalPevalFromFPOrTerm d _ (ConTerm f) | fpIsNaN f || fpIsInfinite f = d
generalPevalFromFPOrTerm d rd f = fromFPOrTerm d rd f

algRealPevalFromFPOrTerm ::
  ( PEvalIEEEFPConvertibleTerm AlgReal,
    ValidFP eb sb,
    IEEEFPConvertible AlgReal (FP eb sb) FPRoundingMode
  ) =>
  Term AlgReal ->
  Term FPRoundingMode ->
  Term (FP eb sb) ->
  Term AlgReal
algRealPevalFromFPOrTerm (ConTerm d) _ (ConTerm f) =
  conTerm $ fromFPOr d RNE f
algRealPevalFromFPOrTerm d _ (ConTerm f) | fpIsNaN f || fpIsInfinite f = d
algRealPevalFromFPOrTerm d _ f = fromFPOrTerm d (conTerm RNE) f

generalDoPevalToFPTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term FPRoundingMode ->
  Term a ->
  Maybe (Term (FP eb sb))
generalDoPevalToFPTerm (ConTerm rd) (ConTerm f) =
  Just $ conTerm $ toFP rd f
generalDoPevalToFPTerm _ _ = Nothing

generalPevalToFPTerm ::
  ( PEvalIEEEFPConvertibleTerm a,
    ValidFP eb sb,
    IEEEFPConvertible a (FP eb sb) FPRoundingMode
  ) =>
  Term FPRoundingMode ->
  Term a ->
  Term (FP eb sb)
generalPevalToFPTerm = binaryUnfoldOnce generalDoPevalToFPTerm toFPTerm

fpDoPevalToFPTerm ::
  ( ValidFP eb sb,
    ValidFP eb1 sb1,
    IEEEFPConvertible (FP eb1 sb1) (FP eb sb) FPRoundingMode
  ) =>
  Term FPRoundingMode ->
  Term (FP eb1 sb1) ->
  Maybe (Term (FP eb sb))
fpDoPevalToFPTerm (ConTerm rd) (ConTerm f) =
  Just $ conTerm $ toFP rd f
fpDoPevalToFPTerm _ (ConTerm f)
  | fpIsNaN f = Just $ conTerm fpNaN
  | fpIsPositiveInfinite f = Just $ conTerm fpPositiveInfinite
  | fpIsNegativeInfinite f = Just $ conTerm fpNegativeInfinite
  | fpIsPositiveZero f = Just $ conTerm fpPositiveZero
  | fpIsNegativeZero f = Just $ conTerm fpNegativeZero
fpDoPevalToFPTerm _ _ = Nothing

fpPevalToFPTerm ::
  ( ValidFP eb sb,
    ValidFP eb1 sb1,
    IEEEFPConvertible (FP eb1 sb1) (FP eb sb) FPRoundingMode
  ) =>
  Term FPRoundingMode ->
  Term (FP eb1 sb1) ->
  Term (FP eb sb)
fpPevalToFPTerm = binaryUnfoldOnce fpDoPevalToFPTerm toFPTerm

instance PEvalIEEEFPConvertibleTerm Integer where
  pevalFromFPOrTerm = generalPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm d mode l =
    SBV.ite (SBV.fpIsNaN l SBV..|| SBV.fpIsInfinite l) d $
      let r = SBV.fromSFloatingPoint mode l :: SBV.SReal
          ifloor = SBV.sRealToSInteger r
          diff = r - SBV.sFromIntegral ifloor
       in SBV.ite (diff SBV..== 0) ifloor
            $ SBV.ite (mode SBV..== SBV.sRTN) ifloor
            $ SBV.ite (mode SBV..== SBV.sRTP) (ifloor + 1)
            $ SBV.ite
              (mode SBV..== SBV.sRTZ)
              (SBV.ite (ifloor SBV..< 0) (ifloor + 1) ifloor)
            $ SBV.ite
              (diff SBV..== 0.5)
              ( SBV.ite
                  (mode SBV..== SBV.sRNE)
                  ( SBV.ite
                      (SBV.sMod ifloor 2 SBV..== 0)
                      ifloor
                      (ifloor + 1)
                  )
                  (SBV.ite (ifloor SBV..< 0) ifloor (ifloor + 1))
              )
            $ SBV.ite
              (diff SBV..< 0.5)
              ifloor
              (ifloor + 1)
  sbvToFPTerm mode l =
    case SBV.unliteral l of
      Nothing -> SBV.toSFloatingPoint mode l
      Just _ ->
        error $
          "SBV's toSFloatingPoint does not regard the rounding mode for "
            ++ "integers. This should never be called. "

instance PEvalIEEEFPConvertibleTerm AlgReal where
  pevalFromFPOrTerm = algRealPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm d mode l =
    SBV.ite (SBV.fpIsNaN l SBV..|| SBV.fpIsInfinite l) d $
      SBV.fromSFloatingPoint mode l
  sbvToFPTerm rm l =
    case SBV.unliteral l of
      Nothing -> SBV.toSFloatingPoint rm l
      Just _ ->
        error $
          "SBV is buggy on converting literal AlgReal to an FP. "
            ++ "This should never be called. "
            ++ "https://github.com/LeventErkok/sbv/pull/718"

instance (KnownNat n, 1 <= n) => PEvalIEEEFPConvertibleTerm (WordN n) where
  pevalFromFPOrTerm = generalPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm = bvIsNonZeroFromGEq1 (Proxy @n) $ boundedSBVFromFPTerm @WordN
  sbvToFPTerm mode l = bvIsNonZeroFromGEq1 (Proxy @n) $ genericFPCast mode l

instance (KnownNat n, 1 <= n) => PEvalIEEEFPConvertibleTerm (IntN n) where
  pevalFromFPOrTerm = generalPevalFromFPOrTerm
  pevalToFPTerm = generalPevalToFPTerm
  sbvFromFPOrTerm = bvIsNonZeroFromGEq1 (Proxy @n) $ boundedSBVFromFPTerm @IntN
  sbvToFPTerm mode l = bvIsNonZeroFromGEq1 (Proxy @n) $ genericFPCast mode l

instance (ValidFP eb sb) => PEvalIEEEFPConvertibleTerm (FP eb sb) where
  pevalFromFPOrTerm _ = fpPevalToFPTerm
  pevalToFPTerm = fpPevalToFPTerm
  sbvFromFPOrTerm _ rm v = case (SBV.unliteral rm, SBV.unliteral v) of
    (Just _, Just _) ->
      error $
        "SBV is buggy on converting literal FP to another FP with different "
          ++ "precision. This should never be called. "
          ++ "https://github.com/LeventErkok/sbv/pull/717"
    _ ->
      SBV.ite (SBV.fpIsNaN v) (conSBVTerm (fpNaN :: FP eb sb)) $
        SBV.toSFloatingPoint rm v
  sbvToFPTerm ::
    forall eb1 sb1.
    (ValidFP eb sb, ValidFP eb1 sb1) =>
    SBVType FPRoundingMode ->
    SBVType (FP eb sb) ->
    SBVType (FP eb1 sb1)
  sbvToFPTerm rm v = case (SBV.unliteral rm, SBV.unliteral v) of
    (Just _, Just _) ->
      error $
        "SBV is buggy on converting literal FP to another FP with different "
          ++ "precision. This should never be called. "
          ++ "https://github.com/LeventErkok/sbv/pull/717"
    _ ->
      SBV.ite (SBV.fpIsNaN v) (conSBVTerm (fpNaN :: FP eb1 sb1)) $
        SBV.toSFloatingPoint rm v
