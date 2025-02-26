{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm () where

import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm,
        withSbvNumTermConstraint
      ),
    SupportedPrim (withPrim),
    absNumTerm,
    addNumTerm,
    doPevalNoOverflowAbsNumTerm,
    doPevalNoOverflowSignumNumTerm,
    mulNumTerm,
    negNumTerm,
    pevalDefaultAddNumTerm,
    pevalDefaultMulNumTerm,
    pevalDefaultNegNumTerm,
    signumNumTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( generalBinaryUnfolded,
    generalUnaryUnfolded,
    unaryUnfoldOnce,
  )

instance PEvalNumTerm Integer where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = unaryUnfoldOnce doPevalNoOverflowAbsNumTerm absNumTerm
  pevalSignumNumTerm =
    unaryUnfoldOnce doPevalNoOverflowSignumNumTerm signumNumTerm
  withSbvNumTermConstraint r = r

instance (ValidFP eb sb) => PEvalNumTerm (FP eb sb) where
  pevalAddNumTerm = generalBinaryUnfolded (+) addNumTerm
  pevalNegNumTerm = generalUnaryUnfolded negate negNumTerm
  pevalMulNumTerm = generalBinaryUnfolded (*) mulNumTerm
  pevalAbsNumTerm = generalUnaryUnfolded abs absNumTerm
  pevalSignumNumTerm = generalUnaryUnfolded signum signumNumTerm
  withSbvNumTermConstraint r = withPrim @(FP eb sb) r

instance PEvalNumTerm AlgReal where
  pevalAddNumTerm = pevalDefaultAddNumTerm
  pevalNegNumTerm = pevalDefaultNegNumTerm
  pevalMulNumTerm = pevalDefaultMulNumTerm
  pevalAbsNumTerm = unaryUnfoldOnce doPevalNoOverflowAbsNumTerm absNumTerm
  pevalSignumNumTerm =
    unaryUnfoldOnce doPevalNoOverflowSignumNumTerm signumNumTerm
  withSbvNumTermConstraint r = withPrim @AlgReal r
