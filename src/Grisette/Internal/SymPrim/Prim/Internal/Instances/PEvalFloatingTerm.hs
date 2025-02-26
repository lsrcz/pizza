{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFloatingTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFloatingTerm () where

import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FloatingUnaryOp (FloatingAcosh, FloatingAsinh, FloatingAtanh, FloatingSqrt),
    PEvalFloatingTerm
      ( pevalFloatingUnaryTerm,
        pevalPowerTerm,
        withSbvFloatingTermConstraint
      ),
    SupportedPrim (withPrim),
    floatingUnaryTerm,
    powerTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( generalUnaryUnfolded,
  )

instance (ValidFP eb sb) => PEvalFloatingTerm (FP eb sb) where
  pevalFloatingUnaryTerm op =
    case op of
      FloatingSqrt -> generalUnaryUnfolded sqrt $ floatingUnaryTerm op
      _ -> error $ "operation " <> show op <> " not supported for FP"
  pevalPowerTerm = error "power operation not supported for FP"
  withSbvFloatingTermConstraint r = withPrim @(FP eb sb) r

instance PEvalFloatingTerm AlgReal where
  pevalFloatingUnaryTerm op =
    case op of
      FloatingAsinh ->
        error "operation asinh not supported by sbv for AlgReal"
      FloatingAcosh ->
        error "operation acosh not supported by sbv for AlgReal"
      FloatingAtanh ->
        error "operation atanh not supported by sbv for AlgReal"
      _ -> floatingUnaryTerm op
  pevalPowerTerm = powerTerm
  withSbvFloatingTermConstraint r = withPrim @AlgReal r
