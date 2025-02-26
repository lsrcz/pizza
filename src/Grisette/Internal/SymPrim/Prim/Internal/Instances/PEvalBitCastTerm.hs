{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitCastTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitCastTerm
  ( doPevalBitCast,
  )
where

import qualified Data.SBV as SBV
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast (bitCast),
    BitCastOr (bitCastOr),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP, withValidFPProofs)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalBitCastOrTerm (pevalBitCastOrTerm, sbvBitCastOr),
    PEvalBitCastTerm (pevalBitCastTerm, sbvBitCast),
    SupportedPrim,
    Term,
    bitCastOrTerm,
    bitCastTerm,
    conTerm,
    pattern BitCastTerm,
    pattern ConTerm,
    pattern DynTerm,
    pattern SupportedTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( binaryUnfoldOnce,
    unaryUnfoldOnce,
  )

doPevalBitCastSameType ::
  forall x b. (SupportedPrim b) => Term x -> Maybe (Term b)
doPevalBitCastSameType (BitCastTerm (DynTerm (b :: Term b))) = Just b
doPevalBitCastSameType (BitCastTerm x) = doPevalBitCastSameType x
doPevalBitCastSameType _ = Nothing

-- | Partially evaluate a bitcast term. If no reduction is performed, return
-- Nothing.
doPevalBitCast :: (PEvalBitCastTerm a b, SupportedPrim b) => Term a -> Maybe (Term b)
doPevalBitCast (ConTerm v) = Just $ conTerm $ bitCast v
doPevalBitCast t = doPevalBitCastSameType t

pevalBitCastGeneral ::
  forall a b.
  (PEvalBitCastTerm a b, SupportedPrim b) =>
  Term a ->
  Term b
pevalBitCastGeneral = unaryUnfoldOnce doPevalBitCast bitCastTerm

doPevalBitCastOr ::
  (PEvalBitCastOrTerm a b) =>
  Term b ->
  Term a ->
  Maybe (Term b)
doPevalBitCastOr (ConTerm d) (ConTerm v) =
  Just $ conTerm $ bitCastOr d v
doPevalBitCastOr _ _ = Nothing

pevalBitCastOr ::
  forall a b.
  (PEvalBitCastOrTerm a b) =>
  Term b ->
  Term a ->
  Term b
pevalBitCastOr d@SupportedTerm =
  binaryUnfoldOnce doPevalBitCastOr bitCastOrTerm d

instance PEvalBitCastTerm Bool (IntN 1) where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast x = SBV.ite x (SBV.literal 1) (SBV.literal 0)

instance PEvalBitCastTerm Bool (WordN 1) where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast x = SBV.ite x (SBV.literal 1) (SBV.literal 0)

instance PEvalBitCastTerm (IntN 1) Bool where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast x = SBV.sTestBit x 0

instance PEvalBitCastTerm (WordN 1) Bool where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast x = SBV.sTestBit x 0

instance
  (n ~ (eb + sb), ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastTerm (WordN n) (FP eb sb)
  where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast = withValidFPProofs @eb @sb $ SBV.sWordAsSFloatingPoint

instance
  (n ~ (eb + sb), ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastTerm (IntN n) (FP eb sb)
  where
  pevalBitCastTerm = pevalBitCastGeneral
  sbvBitCast =
    withValidFPProofs @eb @sb $ SBV.sWordAsSFloatingPoint . SBV.sFromIntegral

instance
  (n ~ (eb + sb), ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastOrTerm (FP eb sb) (WordN n)
  where
  pevalBitCastOrTerm = pevalBitCastOr
  sbvBitCastOr d v =
    withValidFPProofs @eb @sb $
      SBV.ite
        (SBV.fpIsNaN v)
        d
        (SBV.sFloatingPointAsSWord v)

instance
  (n ~ (eb + sb), ValidFP eb sb, KnownNat n, 1 <= n) =>
  PEvalBitCastOrTerm (FP eb sb) (IntN n)
  where
  pevalBitCastOrTerm = pevalBitCastOr
  sbvBitCastOr d v =
    withValidFPProofs @eb @sb $
      SBV.ite
        (SBV.fpIsNaN v)
        d
        (SBV.sFromIntegral $ SBV.sFloatingPointAsSWord v)
