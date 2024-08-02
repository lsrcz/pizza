{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedFP
  ( GetFP,
    GetFPRoundingMode,
    UnifiedFP,
    AllUnifiedFP,
    UnifiedFPImpl,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (Nat)
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants,
    IEEEFPOp,
    IEEEFPRoundingOp,
  )
import Grisette.Internal.Core.Data.Class.SymIEEEFP (SymIEEEFPTraits)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)

class
  ( BasicGrisetteType fp,
    ConSymConversion (FP eb sb) (SymFP eb sb) fp,
    UnifiedPrimitive mode fp,
    Floating fp,
    SymIEEEFPTraits fp,
    IEEEFPConstants fp,
    IEEEFPOp fp,
    IEEEFPRoundingOp fp rd,
    fpn ~ GetFP mode,
    fp ~ fpn eb sb,
    rd ~ GetFPRoundingMode mode
  ) =>
  UnifiedFPImpl (mode :: EvalModeTag) fpn eb sb fp rd
    | fpn eb sb -> fp rd,
      fp -> fpn eb sb rd,
      rd -> fpn,
      rd eb sb -> fp
  where
  -- | Get a unified floating point type. Resolves to 'FP' in 'Con' mode, and
  -- 'SymFP' in 'Sym' mode.
  type GetFP mode = (f :: Nat -> Nat -> Type) | f -> mode

  -- | Get a unified floating point rounding mode type. Resolves to
  -- 'FPRoundingMode' in 'Con' mode, and 'SymFPRoundingMode' in 'Sym' mode.
  type GetFPRoundingMode mode = r | r -> mode

instance
  (ValidFP eb sb) =>
  UnifiedFPImpl 'Con FP eb sb (FP eb sb) FPRoundingMode
  where
  type GetFP 'Con = FP
  type GetFPRoundingMode 'Con = FPRoundingMode

instance
  (ValidFP eb sb) =>
  UnifiedFPImpl 'Sym SymFP eb sb (SymFP eb sb) SymFPRoundingMode
  where
  type GetFP 'Sym = SymFP
  type GetFPRoundingMode 'Sym = SymFPRoundingMode

-- | Evaluation mode with unified 'FP' type.
class
  ( UnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedFP mode eb sb

instance
  ( UnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedFP mode eb sb

-- | Evaluation mode with unified floating point type.
class
  (forall eb sb. (ValidFP eb sb) => UnifiedFP mode eb sb) =>
  AllUnifiedFP mode

instance
  (forall eb sb. (ValidFP eb sb) => UnifiedFP mode eb sb) =>
  AllUnifiedFP mode
