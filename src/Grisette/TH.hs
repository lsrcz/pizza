{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.TH
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.TH
  ( -- * Convenient derivation of all instances relating to Grisette
    EvalModeConfig (..),
    DeriveConfig (..),
    derive,
    deriveWith,
    allClasses0,
    allClasses01,
    allClasses012,
    basicClasses0,
    noExistentialClasses0,
    concreteOrdClasses0,
    basicClasses1,
    noExistentialClasses1,
    concreteOrdClasses1,
    basicClasses2,
    noExistentialClasses2,
    concreteOrdClasses2,
    showClasses,
    pprintClasses,
    evalSymClasses,
    extractSymClasses,
    substSymClasses,
    allSymsClasses,
    eqClasses,
    ordClasses,
    symOrdClasses,
    symEqClasses,
    unifiedSymOrdClasses,
    unifiedSymEqClasses,
    mergeableClasses,
    nfDataClasses,
    hashableClasses,
    toSymClasses,
    toConClasses,
    serialClasses,
    simpleMergeableClasses,
    unifiedSimpleMergeableClasses,
    filterExactNumArgs,
    filterLeqNumArgs,

    -- * Smart constructors that merges in a monad
    makePrefixedSmartCtor,
    makeNamedSmartCtor,
    makeSmartCtor,
    makeSmartCtorWith,

    -- * Smart constructors that are polymorphic in evaluation modes
    makePrefixedUnifiedCtor,
    makeNamedUnifiedCtor,
    makeUnifiedCtor,
    makeUnifiedCtorWith,
  )
where

import Grisette.Internal.TH.Ctor.SmartConstructor
  ( makeNamedSmartCtor,
    makePrefixedSmartCtor,
    makeSmartCtor,
    makeSmartCtorWith,
  )
import Grisette.Internal.TH.Ctor.UnifiedConstructor
  ( makeNamedUnifiedCtor,
    makePrefixedUnifiedCtor,
    makeUnifiedCtor,
    makeUnifiedCtorWith,
  )
import Grisette.Internal.TH.Derivation.Common
  ( DeriveConfig (..),
    EvalModeConfig (..),
  )
import Grisette.Internal.TH.Derivation.Derive
  ( allClasses0,
    allClasses01,
    allClasses012,
    allSymsClasses,
    basicClasses0,
    basicClasses1,
    basicClasses2,
    concreteOrdClasses0,
    concreteOrdClasses1,
    concreteOrdClasses2,
    derive,
    deriveWith,
    eqClasses,
    evalSymClasses,
    extractSymClasses,
    filterExactNumArgs,
    filterLeqNumArgs,
    hashableClasses,
    mergeableClasses,
    nfDataClasses,
    noExistentialClasses0,
    noExistentialClasses1,
    noExistentialClasses2,
    ordClasses,
    pprintClasses,
    serialClasses,
    showClasses,
    simpleMergeableClasses,
    substSymClasses,
    symEqClasses,
    symOrdClasses,
    toConClasses,
    toSymClasses,
    unifiedSimpleMergeableClasses,
    unifiedSymEqClasses,
    unifiedSymOrdClasses,
  )
