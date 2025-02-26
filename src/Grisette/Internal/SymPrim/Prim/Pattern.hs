{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Pattern
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Pattern
  ( pattern SubTerms,
  )
where

import Data.Foldable (Foldable (toList))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( Term,
    pattern AbsNumTerm,
    pattern AddNumTerm,
    pattern AndBitsTerm,
    pattern AndTerm,
    pattern ApplyTerm,
    pattern BVConcatTerm,
    pattern BVExtendTerm,
    pattern BVSelectTerm,
    pattern BitCastOrTerm,
    pattern BitCastTerm,
    pattern ComplementBitsTerm,
    pattern ConTerm,
    pattern DistinctTerm,
    pattern DivIntegralTerm,
    pattern EqTerm,
    pattern ExistsTerm,
    pattern FPBinaryTerm,
    pattern FPFMATerm,
    pattern FPRoundingBinaryTerm,
    pattern FPRoundingUnaryTerm,
    pattern FPTraitTerm,
    pattern FPUnaryTerm,
    pattern FdivTerm,
    pattern FloatingUnaryTerm,
    pattern ForallTerm,
    pattern FromFPOrTerm,
    pattern FromIntegralTerm,
    pattern ITETerm,
    pattern LeOrdTerm,
    pattern LtOrdTerm,
    pattern ModIntegralTerm,
    pattern MulNumTerm,
    pattern NegNumTerm,
    pattern NotTerm,
    pattern OrBitsTerm,
    pattern OrTerm,
    pattern PowerTerm,
    pattern QuotIntegralTerm,
    pattern RecipTerm,
    pattern RemIntegralTerm,
    pattern RotateLeftTerm,
    pattern RotateRightTerm,
    pattern ShiftLeftTerm,
    pattern ShiftRightTerm,
    pattern SignumNumTerm,
    pattern SymTerm,
    pattern ToFPTerm,
    pattern XorBitsTerm,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))

subTermsViewPattern :: Term a -> Maybe [SomeTerm]
subTermsViewPattern (ConTerm _) = return []
subTermsViewPattern (SymTerm _) = return []
subTermsViewPattern (ForallTerm _ t) = return [SomeTerm t]
subTermsViewPattern (ExistsTerm _ t) = return [SomeTerm t]
subTermsViewPattern (NotTerm t) = return [SomeTerm t]
subTermsViewPattern (OrTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (AndTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (EqTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (DistinctTerm t) = return (SomeTerm <$> toList t)
subTermsViewPattern (ITETerm t1 t2 t3) =
  return [SomeTerm t1, SomeTerm t2, SomeTerm t3]
subTermsViewPattern (AddNumTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (NegNumTerm t) = return [SomeTerm t]
subTermsViewPattern (MulNumTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (AbsNumTerm t) = return [SomeTerm t]
subTermsViewPattern (SignumNumTerm t) = return [SomeTerm t]
subTermsViewPattern (LtOrdTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (LeOrdTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (AndBitsTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (OrBitsTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (XorBitsTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (ComplementBitsTerm t) = return [SomeTerm t]
subTermsViewPattern (ShiftLeftTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (ShiftRightTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (RotateLeftTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (RotateRightTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (BitCastTerm t1) = return [SomeTerm t1]
subTermsViewPattern (BitCastOrTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (BVConcatTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (BVSelectTerm _ _ t1) = return [SomeTerm t1]
subTermsViewPattern (BVExtendTerm _ _ t1) = return [SomeTerm t1]
subTermsViewPattern (ApplyTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (DivIntegralTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (ModIntegralTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (QuotIntegralTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (RemIntegralTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (FPTraitTerm _ t1) = return [SomeTerm t1]
subTermsViewPattern (FdivTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (RecipTerm t) = return [SomeTerm t]
subTermsViewPattern (FloatingUnaryTerm _ t) = return [SomeTerm t]
subTermsViewPattern (PowerTerm t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (FPUnaryTerm _ t) = return [SomeTerm t]
subTermsViewPattern (FPBinaryTerm _ t1 t2) = return [SomeTerm t1, SomeTerm t2]
subTermsViewPattern (FPRoundingUnaryTerm _ rd t) = return [SomeTerm rd, SomeTerm t]
subTermsViewPattern (FPRoundingBinaryTerm _ rd t1 t2) = return [SomeTerm rd, SomeTerm t1, SomeTerm t2]
subTermsViewPattern (FPFMATerm rd t1 t2 t3) =
  return [SomeTerm rd, SomeTerm t1, SomeTerm t2, SomeTerm t3]
subTermsViewPattern (FromIntegralTerm t) = return [SomeTerm t]
subTermsViewPattern (FromFPOrTerm t1 rd t2) = return [SomeTerm t1, SomeTerm rd, SomeTerm t2]
subTermsViewPattern (ToFPTerm rd t1 _ _) = return [SomeTerm rd, SomeTerm t1]

-- | Extract all the subterms of a term.
pattern SubTerms :: [SomeTerm] -> Term a
pattern SubTerms ts <- (subTermsViewPattern -> Just ts)

#if MIN_VERSION_base(4, 16, 4)
{-# COMPLETE SubTerms #-}
{-# INLINE SubTerms #-}
#endif
