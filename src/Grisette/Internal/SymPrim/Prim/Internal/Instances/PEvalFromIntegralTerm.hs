{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFromIntegralTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFromIntegralTerm () where

import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFromIntegralTerm (pevalFromIntegralTerm, sbvFromIntegralTerm),
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim,
    Term,
    bvIsNonZeroFromGEq1,
    conTerm,
    fromIntegralTerm,
    pattern ConTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)

pevalFromIntegralTermGeneric ::
  (PEvalFromIntegralTerm a b, SupportedPrim b) => Term a -> Term b
pevalFromIntegralTermGeneric =
  unaryUnfoldOnce doPEvalFromIntegralTerm fromIntegralTerm
  where
    doPEvalFromIntegralTerm (ConTerm a) = Just $ conTerm $ fromIntegral a
    doPEvalFromIntegralTerm _ = Nothing

instance PEvalFromIntegralTerm Integer Integer where
  pevalFromIntegralTerm = id
  sbvFromIntegralTerm = id

instance PEvalFromIntegralTerm Integer AlgReal where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l = withNonFuncPrim @Integer $ SBV.sFromIntegral l

instance (KnownNat n, 1 <= n) => PEvalFromIntegralTerm Integer (WordN n) where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @Integer $
        SBV.sFromIntegral l

instance (KnownNat n, 1 <= n) => PEvalFromIntegralTerm Integer (IntN n) where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @Integer $
        SBV.sFromIntegral l

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

instance (ValidFP eb sb) => PEvalFromIntegralTerm Integer (FP eb sb) where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    withNonFuncPrim @Integer $
      SBV.toSFloatingPoint SBV.sRoundNearestTiesToEven l

instance (KnownNat n, 1 <= n) => PEvalFromIntegralTerm (WordN n) Integer where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @Integer $
        SBV.sFromIntegral l

instance
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  PEvalFromIntegralTerm (WordN n) (WordN m)
  where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      bvIsNonZeroFromGEq1 (Proxy @m) $
        withNonFuncPrim @Integer $
          SBV.sFromIntegral l

instance
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  PEvalFromIntegralTerm (WordN n) (IntN m)
  where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      bvIsNonZeroFromGEq1 (Proxy @m) $
        withNonFuncPrim @Integer $
          SBV.sFromIntegral l

instance (KnownNat n, 1 <= n) => PEvalFromIntegralTerm (WordN n) AlgReal where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1
      (Proxy @n)
      (SBV.sFromIntegral (SBV.sFromIntegral l :: SBV.SInteger))

instance
  (KnownNat n, 1 <= n, ValidFP eb sb) =>
  PEvalFromIntegralTerm (WordN n) (FP eb sb)
  where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      genericFPCast SBV.sRoundNearestTiesToEven l

instance (KnownNat n, 1 <= n) => PEvalFromIntegralTerm (IntN n) Integer where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @Integer $
        SBV.sFromIntegral l

instance
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  PEvalFromIntegralTerm (IntN n) (WordN m)
  where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      bvIsNonZeroFromGEq1 (Proxy @m) $
        withNonFuncPrim @Integer $
          SBV.sFromIntegral l

instance
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  PEvalFromIntegralTerm (IntN n) (IntN m)
  where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      bvIsNonZeroFromGEq1 (Proxy @m) $
        withNonFuncPrim @Integer $
          SBV.sFromIntegral l

instance (KnownNat n, 1 <= n) => PEvalFromIntegralTerm (IntN n) AlgReal where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1
      (Proxy @n)
      (SBV.sFromIntegral (SBV.sFromIntegral l :: SBV.SInteger))

instance
  (KnownNat n, 1 <= n, ValidFP eb sb) =>
  PEvalFromIntegralTerm (IntN n) (FP eb sb)
  where
  pevalFromIntegralTerm = pevalFromIntegralTermGeneric
  sbvFromIntegralTerm l =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      genericFPCast SBV.sRoundNearestTiesToEven l
