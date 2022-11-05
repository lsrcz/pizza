{-# LANGUAGE RankNTypes #-}

module Grisette.Lib.Control.Monad where

import Control.Monad
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable

mrgReturnWithStrategy :: (MonadUnion bool u) => GMergingStrategy bool a -> a -> u a

-- | '>>=' with 'Mergeable' knowledge propagation.
mrgBindWithStrategy :: (MonadUnion bool u) => GMergingStrategy bool b -> u a -> (a -> u b) -> u b

-- | 'return' with 'Mergeable' knowledge propagation.
mrgReturn :: (MonadUnion bool u, GMergeable bool a) => a -> u a

-- | '>>=' with 'Mergeable' knowledge propagation.
(>>=~) :: (MonadUnion bool u, GMergeable bool b) => u a -> (a -> u b) -> u b
mrgFoldM :: (MonadUnion bool m, GMergeable bool b, Foldable t) => (b -> a -> m b) -> b -> t a -> m b

-- | '>>' with 'Mergeable' knowledge propagation.
--
-- This is usually more efficient than calling the original '>>' and merge the results.
(>>~) :: forall bool m a b. (SymBoolOp bool, MonadUnion bool m, GMergeable bool b) => m a -> m b -> m b

-- | 'mzero' with 'Mergeable' knowledge propagation.
mrgMzero :: forall bool m a. (MonadUnion bool m, GMergeable bool a, MonadPlus m) => m a

-- | 'mplus' with 'Mergeable' knowledge propagation.
mrgMplus :: forall bool m a. (MonadUnion bool m, GMergeable bool a, MonadPlus m) => m a -> m a -> m a

-- | 'fmap' with 'Mergeable' knowledge propagation.
mrgFmap :: (MonadUnion bool f, GMergeable bool b, Functor f) => (a -> b) -> f a -> f b
