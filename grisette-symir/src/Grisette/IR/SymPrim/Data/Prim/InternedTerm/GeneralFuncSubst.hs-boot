{-# LANGUAGE RankNTypes #-}

module Grisette.IR.SymPrim.Data.Prim.InternedTerm.GeneralFuncSubst (generalFuncSubst) where

import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term

generalFuncSubst :: forall a b. (SupportedPrim a, SupportedPrim b) => TypedSymbol a -> Term a -> Term b -> Term b
