{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Lib.Data.Either (mrgLeft, mrgRight) where

import Grisette.Internal.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Internal.TH.MergeConstructor
  ( mkMergeConstructor,
  )

mkMergeConstructor "mrg" ''Either
