{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.Unified.Class.UnifiedSafeBitCast
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Class.UnifiedSafeBitCast
  ( safeBitCast,
    UnifiedSafeBitCast (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.SafeBitCast (SafeBitCast)
import qualified Grisette.Internal.Core.Data.Class.SafeBitCast
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, NotRepresentableFPError, ValidFP)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S))
import Grisette.Internal.Unified.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SafeLinearArith.safeSub`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSub @mode a b
safeBitCast ::
  forall mode e a b m.
  ( MonadError e m,
    UnifiedSafeBitCast mode e a b m
  ) =>
  a ->
  m b
safeBitCast a =
  withBaseSafeBitCast @mode @e @a @b @m $
    Grisette.Internal.Core.Data.Class.SafeBitCast.safeBitCast a
{-# INLINE safeBitCast #-}

-- | A class that provides unified safe bitcast operations.
--
-- We use this type class to help resolve the constraints for `SafeBitCast`.
class UnifiedSafeBitCast (mode :: EvalModeTag) e a b m where
  withBaseSafeBitCast :: ((SafeBitCast e a b m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeBitCast e a b m) =>
  UnifiedSafeBitCast mode e a b m
  where
  withBaseSafeBitCast r = r

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching mode m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ (eb + sb)
  ) =>
  UnifiedSafeBitCast mode NotRepresentableFPError (FP eb sb) (WordN n) m
  where
  withBaseSafeBitCast r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching mode m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ (eb + sb)
  ) =>
  UnifiedSafeBitCast mode NotRepresentableFPError (FP eb sb) (IntN n) m
  where
  withBaseSafeBitCast r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching 'S m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ (eb + sb)
  ) =>
  UnifiedSafeBitCast 'S NotRepresentableFPError (SymFP eb sb) (SymWordN n) m
  where
  withBaseSafeBitCast r = withBaseBranching @'S @m r

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching 'S m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ (eb + sb)
  ) =>
  UnifiedSafeBitCast 'S NotRepresentableFPError (SymFP eb sb) (SymIntN n) m
  where
  withBaseSafeBitCast r = withBaseBranching @'S @m r
