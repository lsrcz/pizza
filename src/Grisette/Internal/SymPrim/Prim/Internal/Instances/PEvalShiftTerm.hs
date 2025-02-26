{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalShiftTerm
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalShiftTerm
  ( pevalFiniteBitsSymShiftShiftLeftTerm,
    pevalFiniteBitsSymShiftShiftRightTerm,
  )
where

import Data.Bits (Bits (isSigned, shiftR, zeroBits), FiniteBits (finiteBitSize))
import Data.Proxy (Proxy (Proxy))
import Data.Typeable ((:~:) (Refl))
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.SymShift (SymShift (symShift))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalBVTerm (pevalBVConcatTerm, pevalBVExtendTerm),
    PEvalShiftTerm
      ( pevalShiftLeftTerm,
        pevalShiftRightTerm,
        withSbvShiftTermConstraint
      ),
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim,
    Term,
    bvIsNonZeroFromGEq1,
    conTerm,
    shiftLeftTerm,
    shiftRightTerm,
    unsafePevalBVSelectTerm,
    pattern ConTerm,
    pattern SupportedTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold (unaryUnfoldOnce)
import Grisette.Internal.Utils.Parameterized
  ( LeqProof (LeqProof),
    NatRepr,
    SomePositiveNatRepr (SomePositiveNatRepr),
    mkPositiveNatRepr,
    natRepr,
    subNat,
    unsafeAxiom,
    unsafeLeqProof,
  )

-- | Partial evaluation of symbolic shift left term for finite bits types.
pevalFiniteBitsSymShiftShiftLeftTerm ::
  forall bv n.
  ( forall m. (KnownNat m, 1 <= m) => Integral (bv m),
    forall m. (KnownNat m, 1 <= m) => SymShift (bv m),
    forall m. (KnownNat m, 1 <= m) => SupportedPrim (bv m),
    forall m. (KnownNat m, 1 <= m) => PEvalShiftTerm (bv m),
    PEvalBVTerm bv,
    KnownNat n,
    1 <= n
  ) =>
  Term (bv n) ->
  Term (bv n) ->
  Term (bv n)
pevalFiniteBitsSymShiftShiftLeftTerm t@SupportedTerm n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymShiftShiftLeftTerm` n)
    (`shiftLeftTerm` n)
    t

doPevalFiniteBitsSymShiftShiftLeftTerm ::
  forall bv n.
  ( forall m. (KnownNat m, 1 <= m) => Integral (bv m),
    forall m. (KnownNat m, 1 <= m) => SymShift (bv m),
    forall m. (KnownNat m, 1 <= m) => SupportedPrim (bv m),
    forall m. (KnownNat m, 1 <= m) => PEvalShiftTerm (bv m),
    PEvalBVTerm bv,
    KnownNat n,
    1 <= n
  ) =>
  Term (bv n) ->
  Term (bv n) ->
  Maybe (Term (bv n))
doPevalFiniteBitsSymShiftShiftLeftTerm (ConTerm a) (ConTerm n)
  | n >= 0 =
      if (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n)
        then Just $ conTerm zeroBits
        else Just $ conTerm $ symShift a n
doPevalFiniteBitsSymShiftShiftLeftTerm x (ConTerm 0) = Just x
-- TODO: Need to handle the overflow case.
-- doPevalShiftLeftTerm (ShiftLeftTerm _ x (ConTerm  n)) (ConTerm  n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalShiftLeftTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymShiftShiftLeftTerm _ (ConTerm n)
  | n > 0 && (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n) =
      Just $ conTerm zeroBits
doPevalFiniteBitsSymShiftShiftLeftTerm x (ConTerm shiftAmount)
  | shiftAmount > 0 =
      case (namount, nremaining) of
        ( SomePositiveNatRepr (_ :: NatRepr amount),
          SomePositiveNatRepr (nremaining :: NatRepr remaining)
          ) ->
            case ( unsafeLeqProof @remaining @n,
                   unsafeAxiom @(remaining + amount) @n
                 ) of
              (LeqProof, Refl) ->
                Just $
                  pevalBVConcatTerm
                    (unsafePevalBVSelectTerm nn (natRepr @0) nremaining x)
                    (conTerm zeroBits :: Term (bv amount))
  where
    nn = natRepr @n
    namount = mkPositiveNatRepr $ fromIntegral shiftAmount
    nremaining =
      mkPositiveNatRepr $
        fromIntegral (finiteBitSize shiftAmount) - fromIntegral shiftAmount
doPevalFiniteBitsSymShiftShiftLeftTerm _ _ = Nothing

-- | Partial evaluation of symbolic shift right term for finite bits types.
pevalFiniteBitsSymShiftShiftRightTerm ::
  forall bv n.
  ( forall m. (KnownNat m, 1 <= m) => Integral (bv m),
    forall m. (KnownNat m, 1 <= m) => SymShift (bv m),
    forall m. (KnownNat m, 1 <= m) => SupportedPrim (bv m),
    forall m. (KnownNat m, 1 <= m) => PEvalShiftTerm (bv m),
    PEvalBVTerm bv,
    KnownNat n,
    1 <= n
  ) =>
  Term (bv n) ->
  Term (bv n) ->
  Term (bv n)
pevalFiniteBitsSymShiftShiftRightTerm t@SupportedTerm n =
  unaryUnfoldOnce
    (`doPevalFiniteBitsSymShiftShiftRightTerm` n)
    (`shiftRightTerm` n)
    t

doPevalFiniteBitsSymShiftShiftRightTerm ::
  forall bv n.
  ( forall m. (KnownNat m, 1 <= m) => Integral (bv m),
    forall m. (KnownNat m, 1 <= m) => SymShift (bv m),
    forall m. (KnownNat m, 1 <= m) => SupportedPrim (bv m),
    forall m. (KnownNat m, 1 <= m) => PEvalShiftTerm (bv m),
    PEvalBVTerm bv,
    KnownNat n,
    1 <= n
  ) =>
  Term (bv n) ->
  Term (bv n) ->
  Maybe (Term (bv n))
doPevalFiniteBitsSymShiftShiftRightTerm (ConTerm a) (ConTerm n)
  | n >= 0 && not (isSigned a) =
      if (fromIntegral n :: Integer) >= fromIntegral (finiteBitSize n)
        then Just $ conTerm zeroBits
        else Just $ conTerm $ shiftR a (fromIntegral n)
doPevalFiniteBitsSymShiftShiftRightTerm (ConTerm a) (ConTerm n)
  -- if n >= 0 then -n must be in the range
  | n >= 0 = Just $ conTerm $ symShift a (-n)
doPevalFiniteBitsSymShiftShiftRightTerm x (ConTerm 0) = Just x
-- doPevalFiniteBitsSymShiftShiftRightTerm (ShiftRightTerm _ x (ConTerm  n)) (ConTerm  n1)
--   | n >= 0 && n1 >= 0 = Just $ pevalFiniteBitsSymShiftShiftRightTerm x (conTerm $ n + n1)
doPevalFiniteBitsSymShiftShiftRightTerm x (ConTerm shiftAmount)
  | not (isSigned shiftAmount)
      && (fromIntegral shiftAmount :: Integer) >= fromIntegral (finiteBitSize shiftAmount) =
      Just $ conTerm zeroBits
  | isSigned shiftAmount
      && (fromIntegral shiftAmount :: Integer) >= fromIntegral (finiteBitSize shiftAmount) =
      Just $ pevalBVExtendTerm True nn $ unsafePevalBVSelectTerm nn nnp1 none x
  where
    nn = natRepr @n
    none = natRepr @1
    nnp1 = subNat nn none
doPevalFiniteBitsSymShiftShiftRightTerm x (ConTerm shiftAmount)
  | shiftAmount > 0 =
      case (namount, nremaining) of
        ( SomePositiveNatRepr namount,
          SomePositiveNatRepr (nremaining :: NatRepr remaining)
          ) ->
            case unsafeLeqProof @remaining @n of
              LeqProof ->
                Just $
                  pevalBVExtendTerm (isSigned shiftAmount) nn $
                    unsafePevalBVSelectTerm nn namount nremaining x
  where
    nn = natRepr @n
    namount = mkPositiveNatRepr $ fromIntegral shiftAmount
    nremaining =
      mkPositiveNatRepr $
        fromIntegral (finiteBitSize shiftAmount) - fromIntegral shiftAmount
doPevalFiniteBitsSymShiftShiftRightTerm _ _ = Nothing

instance (KnownNat n, 1 <= n) => PEvalShiftTerm (IntN n) where
  pevalShiftLeftTerm = pevalFiniteBitsSymShiftShiftLeftTerm
  pevalShiftRightTerm = pevalFiniteBitsSymShiftShiftRightTerm
  withSbvShiftTermConstraint r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(IntN n) r

instance (KnownNat n, 1 <= n) => PEvalShiftTerm (WordN n) where
  pevalShiftLeftTerm = pevalFiniteBitsSymShiftShiftLeftTerm
  pevalShiftRightTerm = pevalFiniteBitsSymShiftShiftRightTerm
  withSbvShiftTermConstraint r =
    bvIsNonZeroFromGEq1 (Proxy @n) $
      withNonFuncPrim @(WordN n) r
