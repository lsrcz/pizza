{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :   Grisette.Unified.Internal.EvaluationMode
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (..),
    IsConMode,
  )
where

import Language.Haskell.TH.Syntax (Lift)

-- | Evaluation mode for unified types. 'Con' means concrete evaluation, 'Sym'
-- means symbolic evaluation.
data EvaluationMode = Con | Sym deriving (Lift)

-- | Type family to check if a mode is 'Con'.
type family IsConMode (mode :: EvaluationMode) = (r :: Bool) | r -> mode where
  IsConMode 'Con = 'True
  IsConMode 'Sym = 'False
