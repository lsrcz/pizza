{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymOrd
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymOrd
  ( UnifiedSymOrd (..),
    UnifiedSymOrd1 (..),
    UnifiedSymOrd2 (..),
  )
where

import Data.Functor.Classes (Ord1, Ord2)
import Data.Type.Bool (If)
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
  ( SymOrd,
    SymOrd1,
    SymOrd2,
  )
import Grisette.Internal.Unified.EvalModeTag (IsConMode)

-- | A class that provides unified comparison.
--
-- We use this type class to help resolve the constraints for `Ord` and
-- `SymOrd`.
class UnifiedSymOrd mode a where
  withBaseSymOrd :: (((If (IsConMode mode) (Ord a) (SymOrd a)) => r)) -> r

-- | A class that provides unified lifting of comparison.
--
-- We use this type class to help resolve the constraints for `Ord1` and
-- `SymOrd1`.
class UnifiedSymOrd1 mode f where
  withBaseSymOrd1 :: (((If (IsConMode mode) (Ord1 f) (SymOrd1 f)) => r)) -> r

-- | A class that provides unified lifting of comparison.
--
-- We use this type class to help resolve the constraints for `Ord2` and
-- `SymOrd2`.
class UnifiedSymOrd2 mode f where
  withBaseSymOrd2 :: (((If (IsConMode mode) (Ord2 f) (SymOrd2 f)) => r)) -> r
